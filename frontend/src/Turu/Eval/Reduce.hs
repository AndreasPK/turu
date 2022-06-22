{-# LANGUAGE MultiParamTypeClasses #-}

module Turu.Eval.Reduce where

import Turu.Prelude as P

import Turu.AST
import Turu.AST.Name
import Turu.Pretty

import Control.Applicative
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Char
import Data.Coerce
import Data.Map.Strict as M
import Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)
import Turu.AST.Utils

----------------------------------------
-------- Book keeping stuff
----------------------------------------

data EvalState = EvalState
    { var_heap :: (Map Name Closure)
    , var_cons :: Set Name
    , var_top :: (Map Name Closure)
    , next_unique :: VUnique
    -- ^ Sometimes we need to bind a closure to a variable, we use these to make up a name
    }

addCon :: Name -> EM ()
addCon name = do
    s <- get
    put $ s{var_cons = S.insert name (var_cons s)}

addTopBind :: Name -> (Expr Var) -> EM ()
addTopBind name rhs = do
    s <- get
    rhs' <- stepExpr 9999 rhs
    put $ s{var_top = M.insert name rhs' (var_top s), var_heap = M.insert name rhs' (var_heap s)}

addTopBindsRec :: [(Var, VExpr)] -> EM ()
addTopBindsRec _pairs = error "TODO"
{- ^ This will be tricky, we want to execute each rhs with all other rhss already bound.
 Probably needs some knot-tying to make this work.
-}

initEvalState :: EvalState
initEvalState = EvalState mempty mempty mempty 0

newtype EM a = EM (State EvalState a)
    deriving (Functor, Applicative, Monad)

alt_em :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
alt_em a b = do
    a' <- a
    if isJust a'
        then return a'
        else b

instance MonadState EvalState (EM) where
    put s = EM $ put s
    get = EM $ get

runEM :: EM a -> a
runEM act = fst $ runState (coerce $ act) (initEvalState)

type Code = Expr Var
type Data = Map Name Closure

-- No thunks here! The madness of eager languages
data Closure
    = FunClosure {closure_code :: Code, closure_data :: Data}
    | Fun {closure_code :: Code}
    | -- | Objects have no fvs
      Obj Literal
    | -- | a fully applied constructor
      SatCon
        Var
        -- ^ The constructor being applied
        [Closure]
        -- ^ The arguments to the constructor
    deriving (Eq, Show)

noData :: Data
noData = mempty

mkTmpVar :: EM Var
mkTmpVar = do
    s@EvalState{next_unique} <- get
    put s{next_unique = next_unique + 1}
    -- var = !EvalUnit:x_<next_unique>
    return $ MkVar{v_unique = next_unique, v_name = mkName "!EvalUnit" ("x_" <> T.pack (show next_unique)), v_info = simpValInfo}

----------------------------------------
-------- Actual evaluation
----------------------------------------

evalWithUnit :: Expr Var -> CompilationUnit Var -> Closure
evalWithUnit expr unit = runEM $ do
    stepWithUnit expr unit

evalExpr :: Expr Var -> Closure
evalExpr expr =
    let (r, _s) = runState (coerce $ stepExpr 999999 expr) (initEvalState)
     in r

-- TODO: Closure to expr for testing perhaps?
--- Closure land starts here

-- TODO
stepWithUnit :: Expr Var -> CompilationUnit Var -> EM (Closure)
stepWithUnit expr (Unit{unit_binds, unit_fams}) = do
    addCons
    addBinds

    stepExpr (P.foldr (\c r -> ord c * r) 1 ("Josè" :: String)) expr
  where
    addCons :: EM ()
    addCons = mapM_ addCon $ fmap getName (concat $ fmap getFamCons unit_fams)
    addBinds :: EM ()
    addBinds = mapM_ addBind unit_binds
    addBind (Bind b rhs) = addTopBind (getName b) rhs
    addBind (RecBinds pairs) = addTopBindsRec pairs

getVal :: Name -> EM Closure
getVal name =
    fromMaybe (error $ show name <> " not in lookup scope")
        <$> (getHeapVal name `alt_em` getTopVal name)

getHeapVal :: HasCallStack => Name -> EM (Maybe Closure)
getHeapVal name = do
    s <- get
    let m = var_heap s
        v = M.lookup name m
    return v

getTopVal :: Name -> EM (Maybe Closure)
getTopVal name = do
    m <- var_top <$> get
    pure (M.lookup name m)

captureFvEnv :: Expr Var -> EM Data
captureFvEnv _ = do
    -- TODO: Only capture free variables
    var_heap <$> get

-- Run the computation with v = val and restore old state after
withVarVal :: Name -> Closure -> EM a -> EM a
withVarVal name val act = do
    s <- get
    put $ s{var_heap = M.insert name val (var_heap s)}
    r <- act
    put s
    return r

withVarVals :: [Name] -> [Closure] -> EM a -> EM a
withVarVals [] [] act = act
withVarVals (n : ns) (v : vs) act = do
    withVarVal n v $ withVarVals ns vs act
withVarVals _ _ _ = error "withVarVals: missmatched args"

stepExpr :: HasCallStack => Int -> Code -> EM (Closure)
stepExpr n e
    | n <= 0 = error $ "TODO:stepExpr 0 " ++ ppShow e
stepExpr _ e
    | trace ("stepExpr:" <> ppShow e) False =
        undefined
stepExpr _ (Lit l) = return $ Obj $ l
stepExpr n (App f args) = stepApp n f args
stepExpr _ (Var v)
    | isConName (getName v) =
        pure $
            if conVarArity v == 0
                then SatCon v []
                else Fun $ Var v
    | otherwise = getVal $ getName v
stepExpr _ (Lam b rhs) = FunClosure (Lam b rhs) <$> captureFvEnv rhs -- TODO: Without b
stepExpr n (Let bind body) = evalLet n bind body
stepExpr n (Match scrut alts) = stepMatch n scrut alts

-- Args already evaluated, same for the function, just run the function with the given arguments.
applyClosure :: HasCallStack => Closure -> [Closure] -> EM Closure
applyClosure c [] = pure c
applyClosure _c@Obj{} _ = error "Invalid apply to obj"
applyClosure _c@SatCon{} _ = error "Invalid apply to con"
applyClosure fun args = do
    let fn_code = closure_code fun
    let (arg_bndrs, fn_body) = collectLamBinders fn_code
        -- arg_count = length args
        arity = length arg_bndrs

    let (used_args, extra_args) = P.splitAt arity args
    fun' <- withVarVals (fmap getName arg_bndrs) used_args $ stepExpr 9999999 fn_body
    if P.null extra_args
        then return fun'
        else applyClosure fun' extra_args

-- Evaluate an expression in a given heap context (contained in the monad)
stepApp :: HasCallStack => Int -> Code -> [Code] -> EM (Closure)
stepApp _n f args
    | trace ("stepApp:" <> ppShow (f, args)) False =
        undefined
-- Case1: Constructor applications
stepApp n c@(Var con) args
    -- nullary con
    | isConName (getName con)
    , P.null (getVarConArgs con) =
        assert (P.null args) "Nullary constructor applied to args" $
            return
                ( SatCon
                    con
                    []
                )
    | isConName (getName con) =
        case compare arity (length args) of
            LT -> error "Oversat con app"
            EQ -> SatCon con <$> args' -- TODO: Evaluate argS?
            GT -> do
                -- Since we don't have PAPs (should we?) the result of a partially applied constructor
                -- is a function closure with something like FunClosure (\y z-> Con x y z) [x = <closure>]
                args'' <- args' :: EM [Closure]
                vars <- replicateM arity (mkTmpVar)
                let (present_vars, missing_vars) = P.splitAt n_args vars
                let clo_data = M.fromList $ zip (fmap getName present_vars) args''

                -- arity > arg_count, the result is a function!
                return $ FunClosure (mkLams missing_vars (App c $ fmap Var vars)) clo_data
  where
    args' = mapM (stepExpr (n - 1)) args
    n_args = length args
    -- n_missing = arity - length args
    arity = length (getVarConArgs con)
-- Case 2: Atoms - not sure if they actually arise
stepApp n f [] = stepExpr (n - 1) f
-- Case 3: Generic function application
stepApp n f args = do
    args' <- mapM (stepExpr 99999999) args
    f' <- stepExpr (n - 1) f
    applyClosure f' args'

evalLet :: Int -> Bind Var -> Expr Var -> EM Closure
evalLet _ (RecBinds _binds) _ = error "RecBinds not implemented"
evalLet n (Bind v rhs) body = do
    -- Strict language - strict let
    rhs' <- stepExpr (n - 1) rhs
    -- TODO: Capture the free vs in a closure
    withVarVal (getName v) rhs' $ stepExpr (n - 2) body

stepMatch :: Int -> Var -> [Alt Var] -> EM Closure
stepMatch _n _scrut [] = error "match without alts"
stepMatch n scrut alts = do
    scrut_val <- getVal (getName scrut)
    selectAlt scrut_val
  where
    selectAlt :: Closure -> EM Closure
    selectAlt (Obj l) = do
        let rhss = [rhs | LitAlt alt_lit rhs <- alts, l == alt_lit] ++ [rhs | WildAlt rhs <- alts]
        if P.null rhss
            then error "stepMatch - no alt found"
            else stepExpr (n - 1) $ head rhss
    -- TODO: Probably also needs closure captures treament
    selectAlt (SatCon con args)
        | isConName (getName con)
        , ((rhs, bndrs) : _) <- [(rhs, bndrs) | ConAlt alt_con bndrs rhs <- alts, getName con == getName alt_con] =
            withVarVals (fmap getName bndrs) args $ stepExpr (n - 1) rhs
    selectAlt scrut_val
        -- TODO: Chould check argument is actually a value here
        | (rhs : _) <- [rhs | WildAlt rhs <- alts] =
            stepExpr (n - 1) rhs
        | otherwise = error $ "No matching alts:\n" <> ppShow scrut_val <> "\n" <> ppShow alts
