{-# LANGUAGE RecursiveDo #-}

module Turu.AST.Rename where

import Control.Monad.Trans.State.Strict as ST
import Data.Char as C
import Data.HashMap.Strict as HM
import Data.Text as T
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Var
import Turu.Tc.Type
import Turu.Builtins (anyFam, anyFamName, anyFamV, renameBuiltin)
import Turu.Prelude as P

type RnM a = State RnState a

runRn :: RnM a -> a
runRn act =
    let act' = do
            addFam anyFamName anyFam
            act
     in fst $ runState act' (initRnState)

initRnState :: RnState
initRnState = RnState 0 mempty mempty mempty

renameUnit :: CompilationUnit NameT -> CompilationUnit Var
renameUnit unit = runRn $ rnUnit unit

------------

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

mkArgVarRnInfo :: Name -> VUnique -> RnM IdInfo
mkArgVarRnInfo name _u
    -- Constructor
    | isUpper (T.head $ n_name $ name) =
        error "Constructor args not supported"
    | otherwise =
        return $ VarInfo

-- | Rename binder/argument variables
mkArgVarRn :: NameT -> RnM Var
mkArgVarRn name_t = do
    let name = getName name_t
    u <- nextUniqueM
    info <- mkArgVarRnInfo name u
    let var = MkVar u name info noTy
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
    let fam = HM.lookup name fams
    maybe (nameNotDefined name fams) pure (fam)

getCon :: HasCallStack => Name -> RnM (ConDef Var)
getCon name = do
    constrs <- r_cons <$> get
    let con = HM.lookup name constrs
    maybe (nameNotDefined name constrs) pure (con)

getVar :: HasCallStack => Name -> RnM Var
getVar name
    | isBuiltinName name = pure $ renameBuiltin name
    | otherwise = do
        vars <- r_vars <$> get
        let var = HM.lookup name vars
        maybe (nameNotDefined name vars) pure (var)

nameNotDefined :: HasCallStack => Show mapping => Name -> mapping -> a
nameNotDefined name vars =
    (error $ "Renamer: Key not found:" ++ show name ++ ppShow vars)

--------- Actual rename stuff --------

type FamName = NameT

rnUnit :: CompilationUnit NameT -> RnM (CompilationUnit Var)
rnUnit (Unit name binders fams) = do
    rnDefs <- mapM rnFamDef fams
    rnBnds <- mapM rnBinder' binders
    return $ Unit name rnBnds rnDefs
  where
    rnBinder' b = do
        bnd <- rnBinder b
        let vars = binderVars bnd :: [Var]
        mapM_ (\v -> addVar (getName v) v) vars
        return bnd

rnFamDef :: FamDef NameT -> RnM (FamDef Var)
rnFamDef (FamDef fam_name fam_cons) = mdo
    -- return $! trace $ "rnFam" <> ppShow fam_name
    fam_var <- mkFamVar fam_name fam_cons

    (~con_defs :: [ConDef Var]) <- mapM (rnConDef fam_def fam_name) fam_cons
    fam_def <- return $ FamDef fam_var con_defs :: RnM (FamDef Var)
    addFam (getName fam_name) fam_def
    return $ fam_def
  where

rnConDef :: FamDef Var -> FamName -> ConDef NameT -> RnM (ConDef Var)
rnConDef this_fam_def fam_name def@(ConDef _con_name tag cd_args) = do
    (con_args :: [FamDef Var]) <- mapM getFam' cd_args -- This ties a knot
    con_var <- mkConVar fam_name def con_args
    let rn_def = ConDef (cd_var con_var) tag (P.map fd_var con_args) :: ConDef Var
    return rn_def
  where
    getFam' arg_fam
        | getName fam_name == getName arg_fam = pure this_fam_def
        | otherwise = getFam (getName arg_fam)

-- | Make up a var for this fam, and add it to the env
mkFamVar :: NameT -> [ConDef NameT] -> RnM Var
mkFamVar name _cons = do
    u <- nextUniqueM
    let fam_con = FamCon u 1 (getName name) []
    let info = FamConInfo fam_con
    let fam_var = MkVar{v_unique = u, v_name = (getName name), v_info = info, v_ty = nameT_ty name}
    addVar (getName name) fam_var
    return fam_var

-- | Make up a var for this con, and add it + the con def to the env
mkConVar :: FamName -> (ConDef NameT) -> [FamDef Var] -> RnM (ConDef Var)
mkConVar _fam_name (ConDef con_name con_tag con_arg_tys) arg_defs = do
    u <- nextUniqueM
    let !name = getName con_name
        !ty = nameT_ty con_name
    let con_info = DataCon u con_tag name arg_defs
    let info = FamConInfo con_info
    let con_var = MkVar u name info ty

    -- TODO:
    -- For (Just Bool<0>) this drops the <0> annotation.
    -- Not yet sure of that's ok.
    rn_con_arg_tys <- mapM (getVar . getName) con_arg_tys
    addVar name con_var
    let con_def = ConDef con_var con_tag $ rn_con_arg_tys
    addCon name con_def
    return $ con_def

-- | Renames a non-binder occurence of a name
mkValVarRn :: NameT -> RnM Var
mkValVarRn name_t = do
    !u <- nextUniqueM
    let !name = getName name_t
        !ty = nameT_ty name_t
    -- Todish: Should we look up the type in the env instead?
    -- Probably better to do that in a separate tc pass
    return $ MkVar u name simpValInfo ty

rnBinder :: Bind NameT -> RnM (Bind Var)
rnBinder (Bind name_t rhs) = do
    let !name = getName name_t
        !ty = nameT_ty name_t
    u <- nextUniqueM
    -- unit <- getCurrentUnit
    let v_info = VarInfo
    let var = MkVar u name v_info ty
    Bind var <$> rnRhs rhs
rnBinder (RecBinds pairs) = do
    let (names, rhss) = unzip pairs
    vars <- mapM mkValVarRn names
    rhss' <- withBinders vars $ mapM rnExpr rhss
    pure $ RecBinds $ P.zip vars rhss'

rnRhs :: (Expr NameT) -> RnM (Expr Var)
rnRhs = rnExpr

rnExpr :: HasCallStack => (Expr NameT) -> RnM (Expr Var)
rnExpr (Lit l) = return $ Lit l
rnExpr (Var v) =
    let name = getName v
    in
    if isConName name
        then Var . cd_var <$> (getCon name)
        else Var <$> getVar name
rnExpr (App f args) = do
    f' <- rnExpr f
    args' <- mapM rnExpr args
    return $ App f' args'
rnExpr (Lam b body) = do
    b' <- mkArgVarRn b
    withBinder b' $ do
        Lam b' <$> rnExpr body
rnExpr (Let bind body) = do
    bind' <- rnBinder bind
    let bndrs = binderVars bind'
    body' <- withBinders bndrs $ rnExpr body
    return $ Let bind' body'
rnExpr (Match scrut alts) = rnMatch scrut alts

rnMatch :: NameT -> [Alt NameT] -> RnM (Expr Var)
rnMatch scrut alts = do
    -- Drops the
    scrut' <- getVar (getName scrut) :: (RnM Var)
    alts' <- mapM rnAlt alts
    return $ Match scrut' alts'

rnAlt :: Alt NameT -> RnM (Alt Var)
rnAlt (WildAlt rhs) = WildAlt <$> rnRhs rhs
rnAlt (LitAlt l rhs) = LitAlt l <$> rnRhs rhs
rnAlt (ConAlt con_name bndrs rhs) = do
    con_def <- getCon (c_name con_name)
    let con_var = cd_var con_def
        var_info = v_info con_var
        !con = info_con var_info
    -- con_var <- getVar con_name
    bndrs' <- mapM mkArgVarRn bndrs
    rhs' <- withBinders bndrs' $ rnRhs rhs
    return $ ConAlt con bndrs' rhs'
