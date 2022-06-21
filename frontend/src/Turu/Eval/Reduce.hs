{-# LANGUAGE MultiParamTypeClasses #-}

module Turu.Eval.Reduce where

import Turu.Prelude as P

import Turu.AST
import Turu.AST.Name
import Turu.Pretty

import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Char
import Data.Coerce
import Data.Map.Strict as M
import Data.Set as S
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)

data EvalState = EvalState {var_heap :: (Map Name Closure), var_cons :: Set Name, var_top :: (Map Name (Expr Var))}

addCon :: Name -> EM ()
addCon name = do
    s <- get
    put $ s{var_cons = S.insert name (var_cons s)}

addTopBind :: Name -> (Expr Var) -> EM ()
addTopBind name rhs = do
    s <- get
    put $ s{var_top = M.insert name rhs (var_top s)}

initEvalState :: EvalState
initEvalState = EvalState mempty mempty mempty

newtype EM a = EM (State EvalState a)
    deriving (Functor, Applicative, Monad)

instance MonadState EvalState (EM) where
    put s = EM $ put s
    get = EM $ get

runEM :: EM a -> a
runEM act = fst $ runState (coerce $ act) (initEvalState)

type Code = Expr Var
type Data = Map Name Closure

data Closure = FunClosure Code Data | Fun Code | Obj (Expr Var)

evalWithUnit :: Expr Var -> CompilationUnit Var -> Expr Var
evalWithUnit expr unit = runEM $ stepWithUnit expr unit

evalExpr :: Expr Var -> Expr Var
evalExpr expr =
    let (r, _s) = runState (coerce $ stepExpr 99 expr) (initEvalState)
     in r

--- Closure land starts here

stepWithUnit :: Expr Var -> CompilationUnit Var -> EM (Expr Var)
stepWithUnit expr (Unit{unit_binds, unit_fams}) = do
    addCons
    addBinds

    stepExpr (P.foldr (\c r -> ord c * r) 0 ("Josè" :: String)) expr
  where
    addCons :: EM ()
    addCons = mapM_ addCon $ fmap getName (concat $ fmap getFamCons unit_fams)
    addBinds :: EM ()
    addBinds = mapM_ addBind unit_binds
    addBind (Bind b rhs) = addTopBind (getName b) rhs
    addBind (RecBinds pairs) = mapM_ (\(b, rhs) -> addTopBind (getName b) rhs) pairs

getVal :: HasCallStack => Name -> EM (Expr Var)
getVal name = do
    s <- get
    let m = var_heap s
        v = fromMaybe (error $ "undef var" ++ ppShow name) $ M.lookup name m
    return v

-- Run the computation with v = val and restore old state after
withVarVal :: Name -> Expr Var -> EM a -> EM a
withVarVal name val act = do
    s <- get
    put $ s{var_heap = M.insert name val (var_heap s)}
    r <- act
    put s
    return r

withVarVals :: [Name] -> [Expr Var] -> EM a -> EM a
withVarVals [] [] act = act
withVarVals (n : ns) (v : vs) act = do
    withVarVal n v $ withVarVals ns vs act
withVarVals _ _ _ = error "withVarVals: missmatched args"

stepExpr :: HasCallStack => Int -> Expr Var -> EM (Expr Var)
stepExpr 0 e = return e
stepExpr _ e
    | trace ("stepExpr:" <> ppShow e) False =
        undefined
stepExpr _ (Lit l) = return $ Lit l
stepExpr n (App f args) = stepApp n f args
stepExpr _ (Var v)
    | isConName (getName v) =
        pure $ Var v
    -- TODO: Closure
    | otherwise = getVal $ getName v
stepExpr _ (Lam b rhs) = pure (Lam b rhs)
stepExpr n (Let bind body) = evalLet n bind body
stepExpr n (Match scrut alts) = stepMatch n scrut alts

-- Con x y => Con [eval x] [eval y]
stepApp :: HasCallStack => Int -> Expr Var -> [Expr Var] -> EM (Expr Var)
stepApp n f args
    | trace ("stepApp:" <> ppShow (f, args)) False =
        undefined
stepApp n c@(Var con) args
    | isConName (getName con) =
        App c <$> mapM (stepExpr (n - 1)) args
-- stepApp n f [] = stepExpr (n - 1) f
-- (\x -> body) arg => [eval body{x/[eval body]}]
stepApp n (Lam b rhs) (a : args) = do
    a' <- stepExpr (n - 1) a
    app1 <- withVarVal (getName b) a' (stepExpr (n - 1) rhs)
    traceM $ ppShow app1
    -- Apply next argument if any
    stepApp (n - 2) app1 args
-- f args => eval[f] args

stepApp n f [] = stepExpr (n - 1) f
stepApp n f args = do
    f' <- stepExpr (n - 1) f
    case f' of
        Lam{} -> stepApp (n - 2) f' args
        Var{} ->
            trace "ApplyVar - Builtin case but not implemented" $
                return (App f args)
        _ ->
            trace ("Invalid application:" <> ppShow f <> " " <> ppShow args) $
                return (App f args)

evalLet :: Int -> Bind Var -> Expr Var -> EM (Expr Var)
evalLet _ (RecBinds _binds) _ = error "RecBinds not implemented"
evalLet n (Bind v rhs) body = do
    -- Strict language - strict let
    rhs' <- stepExpr (n - 1) rhs
    -- TODO: Capture the free vs in a closure
    withVarVal (getName v) rhs' $ stepExpr (n - 2) body

stepMatch :: Int -> Var -> [Alt Var] -> EM (Expr Var)
stepMatch _n _scrut [] = error "match without alts"
stepMatch n scrut alts = do
    scrut_val <- getVal (getName scrut)
    selectAlt scrut_val
  where
    selectAlt :: (Expr Var) -> EM (Expr Var)
    selectAlt (Lit l) = do
        let rhss = [rhs | LitAlt alt_lit rhs <- alts, l == alt_lit] ++ [rhs | WildAlt rhs <- alts]
        if P.null rhss
            then error "stepMatch - no alt found"
            else stepExpr (n - 1) $ head rhss
    -- TODO: Probably also needs closure captures treament
    selectAlt (App (Var con) args)
        | isConName (getName con)
        , ((rhs, bndrs) : _) <- [(rhs, bndrs) | ConAlt alt_con bndrs rhs <- alts, getName con == getName alt_con] =
            withVarVals (fmap getName bndrs) args $ stepExpr (n - 1) rhs
    selectAlt _scrut_val
        | (rhs : _) <- [rhs | WildAlt rhs <- alts] =
            stepExpr (n - 1) rhs
        | otherwise = error $ "No matching alts:" <> ppShow scrut <> "\n" <> ppShow alts
