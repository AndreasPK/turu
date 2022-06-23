{-# LANGUAGE RecursiveDo #-}

module Turu.AST.Rename where

import Turu.Prelude as P

import Turu.AST
import Turu.AST.Name

import Control.Monad.Trans.State.Strict as ST
import Data.Char as C
import Data.HashMap.Strict as HM
import Data.Text as T
import GHC.Stack

type RnM a = State RnState a

runRn :: RnM a -> a
runRn act = fst $ runState act (initRnState)

initRnState :: RnState
initRnState = RnState 0 mempty mempty mempty

data RnState = RnState
    { r_unique :: Int
    , r_fams :: HM.HashMap Name (FamDef Var)
    , r_cons :: HM.HashMap Name (ConDef Var)
    , r_vars :: HM.HashMap Name Var
    }

nextUnique :: RnState -> (Int, RnState)
nextUnique s@RnState{r_unique = u} = (u, s{r_unique = u + 1})

nextUniqueM :: RnM Int
nextUniqueM = do
    s <- get
    let (u, s') = nextUnique s
    put s'
    return u

mkArgVarInfo :: Name -> VUnique -> RnM IdInfo
mkArgVarInfo name _u
    -- Constructor
    | isUpper (T.head $ n_name name) =
        error "Constructor args not supported"
    | otherwise =
        return $ VarInfo Nothing

mkArgVar :: Name -> RnM Var
mkArgVar name = do
    u <- nextUniqueM
    info <- mkArgVarInfo name u
    let var = MkVar u name info
    return var

-- Shadow a binder during the given action
withBinder :: Var -> RnM a -> RnM a
withBinder new_var act = do
    s <- get
    let name = getName new_var
        vars = r_vars s
        old_var = HM.lookup name vars
        with_v = s{r_vars = HM.insert name new_var (r_vars s)}

    -- run action with new_var in scope
    put with_v
    r <- act

    -- TODO :: We could split state into re-usable and always changing part
    -- but this seems easier.
    -- and put it out of scope again, potentially restoring the old var.
    let restore = case old_var of
            Nothing -> delete name
            Just var -> insert name var
    s' <- get
    put $ s'{r_vars = restore $ r_vars s'}

    return r

-- Shadow a binder during the given action
withBinders :: [Var] -> RnM a -> RnM a
withBinders [] act = act
withBinders (v : vs) act =
    withBinder v $ withBinders vs act

---------- RN Env Stuff -----------

getCurrentUnit :: RnM UnitName
getCurrentUnit = return "CurrentUnit"

addFam :: Name -> FamDef Var -> RnM ()
addFam name thing = do
    s <- get
    let things = r_fams s
    let things' = HM.insert name thing things
    put $ s{r_fams = things'}

addCon :: Name -> ConDef Var -> RnM ()
addCon name thing = do
    s <- get
    let things = r_cons s
    let things' = HM.insert name thing things
    put $ s{r_cons = things'}

addVar :: Name -> Var -> RnM ()
addVar name thing = do
    s <- get
    let things = r_vars s
    let things' = HM.insert name thing things
    put $ s{r_vars = things'}

getFam :: HasCallStack => Name -> RnM (FamDef Var)
getFam name = do
    fams <- r_fams <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name fams

getCon :: HasCallStack => Name -> RnM (ConDef Var)
getCon name = do
    constrs <- r_cons <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name constrs

getVar :: HasCallStack => Name -> RnM Var
getVar name = do
    vars <- r_vars <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name vars

--------- Actual rename stuff --------

type FamName = Name

rnUnit :: CompilationUnit Name -> RnM (CompilationUnit Var)
rnUnit (Unit name binders fams) = do
    rnDefs <- mapM rnFamDef fams
    rnBnds <- mapM rnBinder binders
    return $ Unit name rnBnds rnDefs

rnFamDef :: FamDef Name -> RnM (FamDef Var)
rnFamDef (FamDef fam_name fam_cons) = mdo
    fam_var <- mkFamVar fam_name fam_cons

    (~con_defs :: [ConDef Var]) <- mapM (rnConDef fam_def fam_name) fam_cons
    fam_def <- return $ FamDef fam_var con_defs :: RnM (FamDef Var)
    return $ fam_def
  where

rnConDef :: FamDef Var -> FamName -> ConDef Name -> RnM (ConDef Var)
rnConDef this_fam_def fam_name def@(ConDef _con_name tag cd_args) = do
    (con_args :: [FamDef Var]) <- mapM getFam' cd_args -- This ties a knot
    con_var <- mkConVar fam_name def con_args
    let rn_def = ConDef (cd_var con_var) tag (P.map fd_var con_args) :: ConDef Var
    return rn_def
  where
    getFam' arg_fam
        | fam_name == arg_fam = pure this_fam_def
        | otherwise = getFam arg_fam

-- | Make up a var for this fam, and add it to the env
mkFamVar :: Name -> [ConDef Name] -> RnM Var
mkFamVar name _cons = do
    u <- nextUniqueM
    let fam_con = FamCon u 1 name []
    let info = FamConInfo fam_con
    let fam_var = MkVar{v_unique = u, v_name = name, v_info = info}
    addVar name fam_var
    return fam_var

-- | Make up a var for this con, and add it + the con def to the env
mkConVar :: FamName -> (ConDef Name) -> [FamDef Var] -> RnM (ConDef Var)
mkConVar _fam_name (ConDef con_name con_tag con_arg_tys) arg_defs = do
    u <- nextUniqueM
    let con_info = DataCon u con_tag con_name arg_defs
    let info = FamConInfo con_info
    -- let info = VarInfo u con_name Nothing unit
    let con_var = MkVar u con_name info
    rn_con_arg_tys <- mapM (getVar) con_arg_tys
    addVar con_name con_var
    let con_def = ConDef con_var con_tag $ rn_con_arg_tys
    addCon con_name con_def
    return $ con_def

mkValVarRn :: Name -> RnM Var
mkValVarRn name = do
    u <- nextUniqueM
    return $ MkVar u name simpValInfo

rnBinder :: Bind Name -> RnM (Bind Var)
rnBinder (Bind name rhs) = do
    u <- nextUniqueM
    -- unit <- getCurrentUnit
    let rhs_unf = Nothing -- We could tie the knot here to store the rhs
    let v_info = VarInfo rhs_unf
    let var = MkVar u name v_info
    Bind var <$> rnRhs rhs
rnBinder (RecBinds pairs) = mdo
    let (names, rhss) = unzip pairs
    vars <- mapM mkValVarRn names
    rhss' <- withBinders vars $ mapM rnExpr rhss
    pure $ RecBinds $ P.zip vars rhss'

rnRhs :: (Expr Name) -> RnM (Expr Var)
rnRhs = rnExpr

rnExpr :: (Expr Name) -> RnM (Expr Var)
rnExpr (Lit l) = return $ Lit l
rnExpr (Var v) =
    if isConName v
        then Var . cd_var <$> (getCon v)
        else Var <$> getVar v
rnExpr (App f args) = do
    f' <- rnExpr f
    args' <- mapM rnExpr args
    return $ App f' args'
rnExpr (Lam b body) = do
    b' <- mkArgVar b
    withBinder b' $ do
        Lam b' <$> rnExpr body
rnExpr (Let bind body) = do
    bind' <- rnBinder bind
    let bndrs = binderVars bind'
    body' <- withBinders bndrs $ rnExpr body
    return $ Let bind' body'
rnExpr (Match scrut alts) = rnMatch scrut alts

rnMatch :: Name -> [Alt Name] -> RnM (Expr Var)
rnMatch scrut alts = do
    scrut' <- getVar scrut :: (RnM Var)
    alts' <- mapM rnAlt alts
    return $ Match scrut' alts'

rnAlt :: Alt Name -> RnM (Alt Var)
rnAlt (WildAlt rhs) = WildAlt <$> rnRhs rhs
rnAlt (LitAlt l rhs) = LitAlt l <$> rnRhs rhs
rnAlt (ConAlt con_name bndrs rhs) = do
    con_var <- getVar con_name
    bndrs' <- mapM mkArgVar bndrs
    rhs' <- withBinders bndrs' $ rnRhs rhs
    return $ ConAlt con_var bndrs' rhs'
