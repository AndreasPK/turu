{-# LANGUAGE RecursiveDo #-}

module Turu.Tc.Infer where

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

type TcM a = State TcState a

runTcM :: TcM a -> a
runTcM act =
    let act' = do
            -- addFam anyFamName anyFam
            act
     in fst $ runState act' (initTcState)

initTcState :: TcState
initTcState = TcState 0 mempty mempty mempty

typecheckUnit :: CompilationUnit NameT -> CompilationUnit Var
typecheckUnit unit = runTcM $ tcUnit unit

------------

data TcState = TcState
    { r_unique :: ~UnitName
    , r_vars :: HM.HashMap Name Ty
    }

nextUnique :: TcState -> (Int, TcState)
nextUnique s@TcState{r_unique = u} = (u, s{r_unique = u + 1})

nextUniqueM :: TcM Int
nextUniqueM = do
    s <- get
    let (u, s') = nextUnique s
    put s'
    return u

mkArgVarRnInfo :: Name -> VUnique -> TcM IdInfo
mkArgVarRnInfo name _u
    -- Constructor
    | isUpper (T.head $ n_name $ name) =
        error "Constructor args not supported"
    | otherwise =
        return $ VarInfo

-- | Rename binder/argument variables
mkArgVarRn :: NameT -> TcM Var
mkArgVarRn name_t = do
    let name = getName name_t
        ty = nameT_ty name_t
    u <- nextUniqueM
    info <- mkArgVarRnInfo name u
    let var = MkVar u name info ty
    return var

-- Shadow a binder during the given action
withBinder :: Var -> TcM a -> TcM a
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
withBinders :: [Var] -> TcM a -> TcM a
withBinders [] act = act
withBinders (v : vs) act =
    withBinder v $ withBinders vs act

---------- RN Env Stuff -----------

getCurrentUnit :: TcM UnitName
getCurrentUnit = return "CurrentUnit"

addFam :: Name -> FamDef Var -> TcM ()
addFam name thing = do
    s <- get
    let things = r_fams s
    let things' = HM.insert name thing things
    put $ s{r_fams = things'}

addCon :: Name -> ConDef Var -> TcM ()
addCon name thing = do
    s <- get
    let things = r_cons s
    let things' = HM.insert name thing things
    put $ s{r_cons = things'}

addVar :: Name -> Var -> TcM ()
addVar name thing = do
    s <- get
    let things = r_vars s
    let things' = HM.insert name thing things
    put $ s{r_vars = things'}

getFam :: HasCallStack => Name -> TcM (FamDef Var)
getFam name = do
    fams <- r_fams <$> get
    let fam = HM.lookup name fams
    maybe (nameNotDefined name fams) pure (fam)

getCon :: HasCallStack => Name -> TcM (ConDef Var)
getCon name = do
    constrs <- r_cons <$> get
    let con = HM.lookup name constrs
    maybe (nameNotDefined name constrs) pure (con)

getVar :: HasCallStack => Name -> TcM Var
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

tcUnit :: CompilationUnit NameT -> TcM (CompilationUnit Var)
tcUnit (Unit name binders fams) = do
    tcDefs <- mapM tcFamDef fams
    tcBnds <- mapM tcBinder' binders
    return $ Unit name tcBnds tcDefs
  where
    tcBinder' b = do
        bnd <- tcBinder b
        let vars = binderVars bnd :: [Var]
        mapM_ (\v -> addVar (getName v) v) vars
        return bnd

tcFamDef :: FamDef NameT -> TcM (FamDef Var)
tcFamDef (FamDef fam_name fam_cons) = mdo
    -- return $! trace $ "rnFam" <> ppShow fam_name
    fam_var <- mkFamVar fam_name fam_cons

    (~con_defs :: [ConDef Var]) <- mapM (tcConDef fam_def fam_name) fam_cons
    fam_def <- return $ FamDef fam_var con_defs :: TcM (FamDef Var)
    addFam (getName fam_name) fam_def
    return $ fam_def
  where

tcConDef :: FamDef Var -> FamName -> ConDef NameT -> TcM (ConDef Var)
tcConDef this_fam_def fam_name def@(ConDef _con_name tag cd_args) = do
    (con_args :: [FamDef Var]) <- mapM getFam' cd_args -- This ties a knot
    con_var <- mkConVar fam_name def con_args
    let rn_def = ConDef (cd_var con_var) tag (P.map fd_var con_args) :: ConDef Var
    return rn_def
  where
    getFam' arg_fam
        | getName fam_name == getName arg_fam = pure this_fam_def
        | otherwise = getFam (getName arg_fam)

-- | Make up a var for this fam, and add it to the env
mkFamVar :: NameT -> [ConDef NameT] -> TcM Var
mkFamVar name _cons = do
    u <- nextUniqueM
    let fam_con = FamCon u 1 (getName name) []
    let info = FamConInfo fam_con
    let fam_var = MkVar{v_unique = u, v_name = (getName name), v_info = info, v_ty = nameT_ty name}
    addVar (getName name) fam_var
    return fam_var

-- | Make up a var for this con, and add it + the con def to the env
mkConVar :: FamName -> (ConDef NameT) -> [FamDef Var] -> TcM (ConDef Var)
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
mkValVarRn :: NameT -> TcM Var
mkValVarRn name_t = do
    !u <- nextUniqueM
    let !name = getName name_t
        !ty = nameT_ty name_t
    -- Todish: Should we look up the type in the env instead?
    -- Probably better to do that in a separate tc pass
    return $ MkVar u name simpValInfo ty

tcBinder :: Bind NameT -> TcM (Bind Var)
tcBinder (Bind name_t rhs) = do
    let !name = getName name_t
        !ty = nameT_ty name_t
    u <- nextUniqueM
    -- unit <- getCurrentUnit
    let v_info = VarInfo
    let var = MkVar u name v_info ty
    Bind var <$> tcRhs rhs
tcBinder (RecBinds pairs) = do
    let (names, rhss) = unzip pairs
    vars <- mapM mkValVarRn names
    rhss' <- withBinders vars $ mapM tcExpr rhss
    pure $ RecBinds $ P.zip vars rhss'

tcRhs :: (Expr NameT) -> TcM (Expr Var)
tcRhs = tcExpr

tcExpr :: HasCallStack => (Expr NameT) -> TcM (Expr Var)
tcExpr (Lit l) = return $ Lit l
tcExpr (Var v) =
    let name = getName v
    in
    if isConName name
        then Var . cd_var <$> (getCon name)
        else Var <$> getVar name
tcExpr (App f args) = do
    f' <- tcExpr f
    args' <- mapM tcExpr args
    return $ App f' args'
tcExpr (Lam b body) = do
    b' <- mkArgVarRn b
    withBinder b' $ do
        Lam b' <$> tcExpr body
tcExpr (Let bind body) = do
    bind' <- tcBinder bind
    let bndrs = binderVars bind'
    body' <- withBinders bndrs $ tcExpr body
    return $ Let bind' body'
tcExpr (Match scrut alts) = tcMatch scrut alts

tcMatch :: NameT -> [Alt NameT] -> TcM (Expr Var)
tcMatch scrut alts = do
    -- Drops the
    scrut' <- getVar (getName scrut) :: (TcM Var)
    alts' <- mapM tcMatch alts
    return $ Match scrut' alts'

tcMatch :: Alt NameT -> TcM (Alt Var)
tcMatch (WildAlt rhs) = WildAlt <$> tcRhs rhs
tcMatch (LitAlt l rhs) = LitAlt l <$> tcRhs rhs
tcMatch (ConAlt con_name bndrs rhs) = do
    con_def <- getCon (c_name con_name)
    let con_var = cd_var con_def
        var_info = v_info con_var
        !con = info_con var_info
    -- con_var <- getVar con_name
    bndrs' <- mapM mkArgVarRn bndrs
    rhs' <- withBinders bndrs' $ tcRhs rhs
    return $ ConAlt con bndrs' rhs'
