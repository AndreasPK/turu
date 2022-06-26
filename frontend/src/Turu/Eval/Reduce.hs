{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Turu.Eval.Reduce where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Bits (Bits (xor))
import Data.Char
import Data.Coerce
import Data.Map.Lazy as M hiding (update)
import Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.Builtins.PrimOps (namePrimOp)
import Turu.Eval.Builtins (evalBuiltin)
import Turu.Eval.Types
import Turu.Prelude as P
import Turu.Pretty

----------------------------------------
-------- Book keeping stuff
----------------------------------------

jose :: a
jose = error "TODO: Jose"

data EvalState = EvalState
    { var_heap :: ~(Map Name Closure)
    , var_cons :: Set Name
    , var_top :: ~(Map Name Closure)
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
    rhs' <- stepExpr doNotInd 9999 rhs
    put $ s{var_top = M.insert name rhs' (var_top s), var_heap = M.insert name rhs' (var_heap s)}

addTopBindsRec :: [(Var, VExpr)] -> EM ()
addTopBindsRec pairs =
    mdo
        let (var_in, rhss_in) = unzip pairs
            var_names = fmap getName var_in
        let inds = fmap Ind var_names :: [Closure]

        rhs_cls <- withVarVals var_names inds $ do
            mapM (stepExpr doNotInd 9999999) rhss_in :: EM [Closure]

        let updateInds :: Data -> Data
        let updateInds m =
                P.foldl' update m $ zip var_names rhs_cls
              where
                update :: Data -> (Name, Closure) -> Data
                update m2 (n, cls) = M.insert n cls m2

        let rhs_cls' = fmap (flip extendClosureEnv updateInds) rhs_cls

        zipWithM_ insertClosure var_in rhs_cls'
  where
    -- couldn't get the mdo trick to work here so we do something "stupid" instead. Which is
    -- construct the closures using a placeholder and then replacing the placeholder inside
    -- the closure environment with the actual thing. But there is a risk that these closures
    -- are lazier than expected as a consequence.

    insertClosure :: Var -> Closure -> EM ()
    insertClosure name rhs' = do
        s <- get
        let name' = getName name
        put $ s{var_top = M.insert name' rhs' (var_top s), var_heap = M.insert name' rhs' (var_heap s)}

    extendClosureEnv :: Closure -> (Data -> Data) -> Closure
    extendClosureEnv c@Obj{} _ = c
    extendClosureEnv c@Builtin{} _ = c
    extendClosureEnv c@Fun{} _ = c
    extendClosureEnv c@(SatCon con cls) upd
        | any isInd cls = SatCon con $ fmap (\c1 -> extendClosureEnv c1 upd) cls
        | otherwise = c
    extendClosureEnv (FunClosure code cls_env) upd = FunClosure code $ upd cls_env
    extendClosureEnv Ind{} _ = error "Waht is happening"

{- | a|b|cons 1  \|cons 2 /
   ^--------------------
-}
initEvalState :: EvalState
initEvalState = EvalState mempty mempty mempty 0

newtype EM a = EM (State EvalState a)
    deriving (Functor, Applicative, Monad, MonadFix)

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

noData :: Data
noData = mempty

mkTmpVar :: EM Var
mkTmpVar = do
    s@EvalState{next_unique} <- get
    put s{next_unique = next_unique + 1}
    -- var = !EvalUnit:x_<next_unique>
    return $ MkVar{v_unique = next_unique, v_name = mkName "!EvalUnit" ("x_" <> T.pack (show next_unique)), v_info = simpValInfo}

isInd :: Closure -> Bool
isInd Ind{} = True
isInd _ = False

----------------------------------------
-------- Actual evaluation
----------------------------------------

evalMain :: CompilationUnit Var -> Either Text Closure
evalMain unit@Unit{unit_binds = binds} =
    let mains = [(v, main) | Bind (v@MkVar{v_name}) main <- binds, n_name v_name == "main"]
     in if P.null mains
            then Left "main function not found"
            else
                let (_v_main, main_rhs) = head mains
                    (arg_bndrs, _main_body) = collectLamBinders main_rhs
                    arity = length arg_bndrs
                    eval_expr = App main_rhs $ replicate arity (Lit $ LitInt 0)
                 in Right $ evalWithUnit eval_expr unit

evalBind :: Text -> CompilationUnit Var -> Either Text Closure
evalBind bind_str unit@Unit{unit_binds = binds} =
    let mains = [(v, main) | Bind (v@MkVar{v_name}) main <- binds, n_name v_name == bind_str]
     in if P.null mains
            then Left "function not found"
            else
                let (_v_main, main_rhs) = head mains
                    (arg_bndrs, _main_body) = collectLamBinders main_rhs
                    arity = length arg_bndrs
                    eval_expr = App main_rhs $ replicate arity (Lit $ LitInt 0)
                 in Right $ evalWithUnit eval_expr unit

evalWithUnit :: Expr Var -> CompilationUnit Var -> Closure
evalWithUnit expr unit = runEM $ do
    stepWithUnit expr unit

evalExpr :: Expr Var -> Closure
evalExpr expr =
    let (r, _s) = runState (coerce $ stepExpr doInd 999999 expr) (initEvalState)
     in r

---------------

doInd, doNotInd :: Bool
doInd = True
doNotInd = False

-- TODO: Closure to expr for testing perhaps?
--- Closure land starts here

-- TODO
stepWithUnit :: Expr Var -> CompilationUnit Var -> EM (Closure)
stepWithUnit expr (Unit{unit_binds, unit_fams}) = do
    addCons
    addBinds

    stepExpr doInd (P.foldr (\c r -> ord c * r) 1 ("Josè" :: String)) expr
  where
    addCons :: EM ()
    addCons = mapM_ addCon $ fmap getName (concat $ fmap getFamCons unit_fams)
    addBinds :: EM ()
    addBinds = mapM_ addBind unit_binds
    addBind (Bind b rhs) = addTopBind (getName b) rhs
    addBind (RecBinds pairs) = addTopBindsRec pairs

getVal :: Name -> EM Closure
getVal name
    | isBuiltinName name = pure $ Builtin $ fromMaybe (error $ "Not a primop:" <> show name) (namePrimOp name)
    | otherwise =
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

stepExpr :: HasCallStack => EvalInd -> Int -> Code -> EM Closure
stepExpr _ n e
    | n <= 0 = error $ "TODO:stepExpr 0 " ++ ppShow e
-- stepExpr _ _ e
--   | trace ("stepExpr:" <> ppShow e) False =
--       undefined
stepExpr _ _ (Lit l) = return $ Obj $ l
stepExpr ind n (App f args) = stepApp ind n f args
stepExpr _ _ (Var v)
    | isConName (getName v) =
        pure $
            if conVarArity v == 0
                then SatCon v []
                else Fun $ Var v
    | otherwise = getVal $ getName v
stepExpr _ _ (Lam b rhs) = FunClosure (Lam b rhs) <$> captureFvEnv rhs -- TODO: Without b
stepExpr ind n (Let bind body) = evalLet ind n bind body
stepExpr ind n (Match scrut alts) = stepMatch ind n scrut alts

type EvalInd = Bool

-- Args already evaluated, same for the function, just run the function with the given arguments.
applyClosure :: HasCallStack => EvalInd -> Closure -> [Closure] -> EM Closure
applyClosure _ (Builtin op) args = pure $ evalBuiltin op args
applyClosure evalInd c@(Ind v) []
    | evalInd = getVal (v)
    | otherwise = pure c
applyClosure evalInd c@(Ind v) args
    | evalInd
    , any isInd args =
        do
            let evalIndArg (Ind v') = getVal (v')
                evalIndArg c' = pure c'
            args' <- mapM evalIndArg args
            c' <- getVal v
            applyClosure evalInd c' args'
    | otherwise = pure c
applyClosure _ c [] = pure c
applyClosure _ _c@Obj{} _ = error "Invalid apply to obj"
applyClosure _ _c@SatCon{} _ = error "Invalid apply to con"
applyClosure evalInd fun args = do
    let fn_code = closure_code fun
    let (arg_bndrs, fn_body) = collectLamBinders fn_code
        -- arg_count = length args
        arity = length arg_bndrs

    let (used_args, extra_args) = P.splitAt arity args
    fun' <- withVarVals (fmap getName arg_bndrs) used_args $ stepExpr evalInd 9999999 fn_body
    if P.null extra_args
        then return fun'
        else applyClosure evalInd fun' extra_args

-- Evaluate an expression in a given heap context (contained in the monad)
stepApp :: HasCallStack => EvalInd -> Int -> Code -> [Code] -> EM Closure
-- stepApp _ _n f args
--   | trace ("stepApp:" <> ppShow (f, args)) False =
--       undefined
-- Case1: Constructor applications
stepApp evalInd n c@(Var con) args
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
            LT -> error $ "Oversat con app" <> (ppShow (arity, length args, con, args))
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
    args' = mapM (stepExpr evalInd (n - 1)) args
    n_args = length args
    -- n_missing = arity - length args
    arity = fromMaybe (error "ConApp not actually con") (getConArity <$> getVarCon con)
-- Case 2: Atoms - not sure if they actually arise
stepApp evalInd n f [] = stepExpr evalInd (n - 1) f
-- Case 3: Generic function application
stepApp evalInd n f args = do
    args' <- mapM (stepExpr evalInd 99999999) args
    f' <- stepExpr evalInd (n - 1) f
    applyClosure evalInd f' args'

evalLet :: EvalInd -> Int -> Bind Var -> Expr Var -> EM Closure
evalLet _ind _ (RecBinds _binds) _ = error "RecBinds not implemented"
evalLet ind n (Bind v rhs) body = do
    -- Strict language - strict let
    rhs' <- stepExpr ind (n - 1) rhs
    -- TODO: Capture the free vs in a closure
    withVarVal (getName v) rhs' $ stepExpr ind (n - 2) body

stepMatch :: EvalInd -> Int -> Var -> [Alt Var] -> EM Closure
stepMatch _ _n _scrut [] = error "match without alts"
stepMatch evalInd n scrut alts = do
    scrut_val <- getVal (getName scrut)
    selectAlt scrut_val
  where
    selectAlt :: Closure -> EM Closure
    selectAlt (Obj l) = do
        let rhss = [rhs | LitAlt alt_lit rhs <- alts, l == alt_lit] ++ [rhs | WildAlt rhs <- alts]
        if P.null rhss
            then error "stepMatch - no alt found"
            else stepExpr evalInd (n - 1) $ head rhss
    -- TODO: Probably also needs closure captures treament
    selectAlt (SatCon con args)
        | isConName (getName con)
        , ((rhs, bndrs) : _) <- [(rhs, bndrs) | ConAlt alt_con bndrs rhs <- alts, getName con == getName alt_con] =
            withVarVals (fmap getName bndrs) args $ stepExpr evalInd (n - 1) rhs
    selectAlt scrut_val
        -- TODO: Chould check argument is actually a value here
        | (rhs : _) <- [rhs | WildAlt rhs <- alts] =
            stepExpr evalInd (n - 1) rhs
        | otherwise = error $ "No matching alts:\n" <> ppShow scrut_val <> "\n" <> ppShow alts
