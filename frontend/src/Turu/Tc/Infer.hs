{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Turu.Builtins (anyFam, anyFamName, anyFamV, renameBuiltin)
import Turu.Prelude as P
import Turu.Tc.Type

type TcM = State TcState

type PostTcVar = Var

type ErrorMsg = Text
data TcState = TcState
    { tcs_unique :: Int
    , tcs_unit :: ~UnitName
    , tcs_vars :: HM.HashMap Name Type
    , tcs_errors :: [ErrorMsg]
    }

failTc :: ErrorMsg -> TcM ()
failTc err = do
    s <- get
    put $ s{tcs_errors = err : tcs_errors s}

runTcM' :: TcM a -> a
runTcM' act =
    either
        (\es -> error $ "Tc failed:" ++ show es)
        (id)
        (runTcM act)

runTcM :: TcM a -> Either [Text] a
runTcM act =
    -- addFam anyFamName anyFam
    let (r, s) = runState act (initTcState)
        errors = tcs_errors s
     in if P.null errors
            then Right r
            else Left errors

initTcState :: TcState
initTcState = TcState 0 ("No Unit initialized") mempty mempty

typecheckUnit :: CompilationUnit Var -> CompilationUnit PostTcVar
typecheckUnit unit = unit -- runTcM' $ tcUnit unit

nextUnique :: TcState -> (Int, TcState)
nextUnique s@TcState{tcs_unique = u} = (u, s{tcs_unique = u + 1})

nextUniqueM :: TcM Int
nextUniqueM = do
    s <- get
    let (u, s') = nextUnique s
    put s'
    return u

------------

mkTcError :: Show a => Text -> a -> TcM ()
mkTcError msg tys = failTc $ msg <> (T.pack $ show tys)

-----------------

mkArgVarRnInfo :: Name -> VUnique -> TcM IdInfo
mkArgVarRnInfo name _u
    -- Constructor
    | isUpper (T.head $ n_name $ name) =
        error "Constructor args not supported"
    | otherwise =
        return $ VarInfo

-- | Rename binder/argument variables
mkArgVarRn :: Var -> TcM PostTcVar
mkArgVarRn name_t = do
    return name_t

-- let name = getName name_t
--     ty = nameT_ty name_t
-- u <- nextUniqueM
-- info <- mkArgVarRnInfo name u
-- let var = MkVar u name info ty
-- return var

-- Shadow a binder during the given action
withBinder :: PostTcVar -> TcM a -> TcM a
withBinder new_var act = do
    act

-- s <- get

-- let name = getName new_var
--     vars = tcs_vars s
--     ty = v_ty new_var
--     old_var = HM.lookup name vars
--     with_v = s{tcs_vars = HM.insert name (Poly [] ty) (tcs_vars s)}

-- -- run action with new_var in scope
-- put with_v
-- r <- act

-- -- TODO :: We could split state into re-usable and always changing part
-- -- but this seems easier.
-- -- and put it out of scope again, potentially restoring the old var.
-- let restore = case old_var of
--         Nothing -> delete name
--         Just var -> insert name var
-- s' <- get
-- put $ s'{tcs_vars = restore $ tcs_vars s'}

-- return r

-- Shadow a binder during the given action
withBinders :: [PostTcVar] -> TcM a -> TcM a
withBinders [] act = act
withBinders (v : vs) act =
    withBinder v $ withBinders vs act

---------- RN Env Stuff -----------

getCurrentUnit :: TcM UnitName
getCurrentUnit = return "CurrentUnit"

-- addFam :: Name -> FamDef PostTcVar -> TcM ()
-- addFam name thing = do
--     s <- get
--     let things = r_fams s
--     let things' = HM.insert name thing things
--     put $ s{r_fams = things'}

-- addCon :: Name -> ConDef PostTcVar -> TcM ()
-- addCon name thing = do
--     s <- get
--     let things = r_cons s
--     let things' = HM.insert name thing things
--     put $ s{r_cons = things'}

addVarTy :: Name -> Var -> TcM ()
addVarTy name thing = do
    undefined

-- s <- get
-- let things = tcs_vars s
-- let things' = HM.insert name thing things
-- put $ s{tcs_vars = things'}

-- getFam :: HasCallStack => Name -> TcM (FamDef PostTcVar)
-- getFam name = do
--     fams <- r_fams <$> get
--     let fam = HM.lookup name fams
--     maybe (nameNotDefined name fams) pure (fam)

-- getCon :: HasCallStack => Name -> TcM (ConDef PostTcVar)
-- getCon name = do
--     constrs <- r_cons <$> get
--     let con = HM.lookup name constrs
--     maybe (nameNotDefined name constrs) pure (con)

-- getVar :: HasCallStack => Name -> TcM PostTcVar
-- getVar name
--     | isBuiltinName name = pure $ renameBuiltin name
--     | otherwise = do
--         vars <- tcs_vars <$> get
--         let var = HM.lookup name vars
--         maybe (nameNotDefined name vars) pure (var)

nameNotDefined :: HasCallStack => Show mapping => Name -> mapping -> a
nameNotDefined name vars =
    (error $ "Renamer: Key not found:" ++ show name ++ ppShow vars)

--------- Actual rename stuff --------

-- type FamName = Var

-- tcUnit :: CompilationUnit Var -> TcM (CompilationUnit PostTcVar)
-- tcUnit (Unit name binders fams) = do
--     tcDefs <- mapM tcFamDef fams
--     tcBnds <- mapM tcBinder' binders
--     return $ Unit name tcBnds tcDefs
--   where
--     tcBinder' b = do
--         bnd <- tcBinder b
--         let vars = binderVars bnd :: [PostTcVar]
--         mapM_ (\v -> addVar (getName v) v) vars
--         return bnd

-- tcFamDef :: FamDef Var -> TcM (FamDef PostTcVar)
-- tcFamDef fam@(FamDef fam_name fam_cons) = mdo
--     return fam
--     -- return $! trace $ "rnFam" <> ppShow fam_name
--     -- fam_var <- mkFamVar fam_name fam_cons

--     -- (~con_defs :: [ConDef PostTcVar]) <- mapM (tcConDef fam_def fam_name) fam_cons
--     -- fam_def <- return $ FamDef fam_var con_defs :: TcM (FamDef PostTcVar)
--     -- addFam (getName fam_name) fam_def
--     -- return $ fam_def
--   where

-- tcConDef :: FamDef PostTcVar -> FamName -> ConDef Var -> TcM (ConDef PostTcVar)
-- tcConDef this_fam_def fam_name def@(ConDef _con_name tag cd_args) = do
--     (con_args :: [FamDef PostTcVar]) <- mapM getFam' cd_args -- This ties a knot
--     con_var <- mkConVar fam_name def con_args
--     let rn_def = ConDef (cd_var con_var) tag (P.map fd_var con_args) :: ConDef PostTcVar
--     return rn_def
--   where
--     getFam' arg_fam
--         | getName fam_name == getName arg_fam = pure this_fam_def
--         | otherwise = getFam (getName arg_fam)

-- -- | Make up a var for this fam, and add it to the env
-- mkFamVar :: Var -> [ConDef Var] -> TcM PostTcVar
-- mkFamVar name _cons = do
--     u <- nextUniqueM
--     let fam_con = FamCon u 1 (getName name) []
--     let info = FamConInfo fam_con
--     let fam_var = MkVar{v_unique = u, v_name = (getName name), v_info = info, v_ty = nameT_ty name}
--     addVar (getName name) fam_var
--     return fam_var

-- -- | Make up a var for this con, and add it + the con def to the env
-- mkConVar :: FamName -> (ConDef Var) -> [FamDef PostTcVar] -> TcM (ConDef PostTcVar)
-- mkConVar _fam_name (ConDef con_name con_tag con_arg_tys) arg_defs = do
--     u <- nextUniqueM
--     let !name = getName con_name
--         !ty = nameT_ty con_name
--     let con_info = DataCon u con_tag name arg_defs
--     let info = FamConInfo con_info
--     let con_var = MkVar u name info ty

--     -- TODO:
--     -- For (Just Bool<0>) this drops the <0> annotation.
--     -- Not yet sure of that's ok.
--     rn_con_arg_tys <- mapM (getVar . getName) con_arg_tys
--     addVar name con_var
--     let con_def = ConDef con_var con_tag $ rn_con_arg_tys
--     addCon name con_def
--     return $ con_def

-- -- | Renames a non-binder occurence of a name
-- mkValVarRn :: Var -> TcM PostTcVar
-- mkValVarRn name_t = do
--     !u <- nextUniqueM
--     let !name = getName name_t
--         !ty = nameT_ty name_t
--     -- Todish: Should we look up the type in the env instead?
--     -- Probably better to do that in a separate tc pass
--     return $ MkVar u name simpValInfo ty

-- tcBinder :: Bind Var -> TcM (Bind PostTcVar)
-- tcBinder (Bind name_t rhs) = do
--     let !name = getName name_t
--         !ty = nameT_ty name_t
--     u <- nextUniqueM
--     -- unit <- getCurrentUnit
--     let v_info = VarInfo
--     let var = MkVar u name v_info ty
--     Bind var <$> tcRhs rhs
-- tcBinder (RecBinds pairs) = do
--     let (names, rhss) = unzip pairs
--     vars <- mapM mkValVarRn names
--     rhss' <- withBinders vars $ mapM tcExpr rhss
--     pure $ RecBinds $ P.zip vars rhss'

-- tcRhs :: (Expr Var) -> TcM (Expr PostTcVar)
-- tcRhs = tcExpr

-- tcExpr :: HasCallStack => (Expr Var) -> TcM (Expr PostTcVar)
-- tcExpr (Lit l) = return $ Lit l
-- tcExpr (Var v) =
--     let name = getName v
--     in
--     if isConName name
--         then Var . cd_var <$> (getCon name)
--         else Var <$> getVar name
-- tcExpr (App f args) = do
--     f' <- tcExpr f
--     args' <- mapM tcExpr args
--     return $ App f' args'
-- tcExpr (Lam b body) = do
--     b' <- mkArgVarRn b
--     withBinder b' $ do
--         Lam b' <$> tcExpr body
-- tcExpr (Let bind body) = do
--     bind' <- tcBinder bind
--     let bndrs = binderVars bind'
--     body' <- withBinders bndrs $ tcExpr body
--     return $ Let bind' body'
-- tcExpr (Match scrut alts) = tcMatch scrut alts

-- tcMatch :: Var -> [Alt Var] -> TcM (Expr PostTcVar)
-- tcMatch scrut alts = do
--     -- Drops the
--     scrut' <- getVar (getName scrut) :: (TcM PostTcVar)
--     alts' <- mapM tcAlt alts
--     return $ Match scrut' alts'

-- tcAlt :: Alt Var -> TcM (Alt PostTcVar)
-- tcAlt (WildAlt rhs) = WildAlt <$> tcRhs rhs
-- tcAlt (LitAlt l rhs) = LitAlt l <$> tcRhs rhs
-- tcAlt (ConAlt con_name bndrs rhs) = do
--     con_def <- getCon (c_name con_name)
--     let con_var = cd_var con_def
--         var_info = v_info con_var
--         !con = info_con var_info
--     -- con_var <- getVar con_name
--     bndrs' <- mapM mkArgVarRn bndrs
--     rhs' <- withBinders bndrs' $ tcRhs rhs
--     return $ ConAlt con bndrs' rhs'
