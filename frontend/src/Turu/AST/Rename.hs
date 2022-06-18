{-# LANGUAGE RecursiveDo #-}

module Turu.AST.Rename where

import Turu.Prelude as P

import Turu.AST

import Control.Monad.Trans.State.Strict as ST
import Data.Char as C
import Data.HashMap.Strict as HM
import Data.Text as T

type RnM a = State RnState a

runRn :: RnM a -> a
runRn act = fst $ runState act (initRnState)

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

mkNewVar :: Text -> RnM Var
mkNewVar name
    -- Constructor Var
    | C.isUpper first =
        undefined
    -- Other var
    | otherwise =
        undefined
  where
    first = T.head name

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

getFam :: Name -> RnM (FamDef Var)
getFam name = do
    fams <- r_fams <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name fams

getCon :: Name -> RnM (ConDef Var)
getCon name = do
    cons <- r_cons <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name cons

getVar :: Name -> RnM Var
getVar name = do
    vars <- r_vars <$> get
    return $ HM.lookupDefault (error $ "Key not found" ++ show name) name vars

--------- Actual rename stuff --------

type FamName = Name

renameUnit :: CompilationUnit Text -> RnM (CompilationUnit Var)
renameUnit (Unit name binders) = Unit name <$> mapM rnBinder binders

rnFamDef :: FamDef Name -> RnM (FamDef Var)
rnFamDef (FamDef fam_name fam_cons) = mdo
    fam_var <- mkFamVar fam_name fam_cons

    (~con_defs :: [ConDef Var]) <- mapM (rnConDef fam_def fam_name) fam_cons
    fam_def <- return $ FamDef fam_var con_defs :: RnM (FamDef Var)
    return $ fam_def
  where

-- -- FOR CONSTRUCTORS JOSE
-- doOneCon :: ConDef Text -> RnM [FamDef Var]
-- doOneCon (ConDef _name _tag arg_names) = mdo
--     (names, args) <- unzip <$> mkConArgs $ P.zip arg_names $ args
--     return $ fmap fst args

-- mkConArgs :: [(FamName, (FamDef Var, ()))] -> RnM [(FamDef Var, ())]
-- mkConArgs = mapM mkConArg

-- mkConArg (arg_fam_name, (future_def, _))
--     | arg_fam_name == fam_name =
--         return (future_def, ())
--     | otherwise = do
--         def <- getFam arg_fam_name
--         return (def, ())

-- mkFamDef :: FamDef Var -> (FamDef Var, FamDef Var)
-- mkFamDef = undefined

-- List = Cons Any List | Nil

rnConDef :: FamDef Var -> Name -> ConDef Name -> RnM (ConDef Var)
rnConDef this_fam_def fam_name def@(ConDef con_name tag cd_args) = do
    (con_args :: [FamDef Var]) <- mapM getFam' cd_args -- Trouble?
    con_var <- mkConVar fam_name def
    let rn_def = ConDef (cd_var con_var) tag (P.map fd_var con_args) :: ConDef Var
    return rn_def
  where
    getFam' arg_fam
        | fam_name == arg_fam = pure this_fam_def
        | otherwise = getFam arg_fam

-- fam Maybe = Just Any | Nothing
mkFamVar :: Name -> [ConDef Name] -> RnM Var
mkFamVar name _cons = do
    u <- nextUniqueM
    unit <- getCurrentUnit
    let fam_con = FamCon u 1 name []
    let info = FamConInfo u name fam_con
    let fam_var = MkVar{v_unique = u, v_unit = unit, v_info = info}
    addVar name fam_var
    return fam_var

mkConVar :: Name -> (ConDef Name) -> RnM (ConDef Var)
mkConVar fam_name (ConDef con_name con_tag con_arg_tys) = do
    u <- nextUniqueM
    unit <- getCurrentUnit
    let info = VarInfo u con_name Nothing unit
    let con_var = MkVar u unit info
    rn_con_arg_tys <- mapM (getFam) con_arg_tys
    addVar con_name con_var
    let con_def = ConDef con_var con_tag $ (P.map fd_var rn_con_arg_tys)
    addCon con_name con_def
    return $ con_def

rnBinder :: Bind Text -> RnM (Bind Var)
rnBinder = undefined
