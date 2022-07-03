module Turu.Bytecode.Expr where

import Turu.Prelude

import Control.Monad.State.Strict
import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.Bytecode.Monad
import Turu.Bytecode.Types

import qualified Data.Map.Strict as M

cgLit :: Literal -> CM [Instruction]
cgLit lit = do
    ref <- addAnonConstantM $ VmLit lit
    return $ [PushRef ref]

-- Name must already be bound in the current context
cgVar :: Var -> CM Instructions
cgVar var
    | Just con <- getVarCon var = do
        error "TODO: cgVar: DataCon"
    -- This should be treated as a function of arity conArity
    -- the saturated case for constructor applications will be handled by cgApp
    | otherwise = pushVar var

cgApp :: Expr Var -> [Expr Var] -> CM Instructions
cgApp fun args = do
    fun_code <- cgFun fun
    arg_code <- sequence $ concatMap cgExpr args
    return $ fun_code <> arg_code <> [Call $ length args]
  where
    cgFun (Var f) = pushVar f
    cgFun _ = cgExpr fun

cgLam :: Var -> VExpr -> CM Instructions
cgLam arg1 body1 = do
    let (args2, body) = collectLamBinders body1
        args = arg1 : args2
        fvs = toList $ freeVarsExpr $ Lam arg1 body1
    cgFunStack args fvs body

-- Compile a function body using the given arguments and capturing the given
-- free variables while ignoring global variables.
-- Puts the resulting function on the stack.
cgFunStack :: [Var] -> [Var] -> VExpr -> CM Instructions
cgFunStack args fvs_all body = do
    let !arity = length args
    globals <- cm_getGlobals
    let fvs = filter (\v -> getName v `M.member` globals) fvs_all
    -- We don't need to include top level things in the closure.
    (body_code, closure_entries, n_locals) <- cgFun args fvs_all body
    body_code_ref <- addAnonConstantM $ VmCode body_code
    push_fvs_code <- concatMapM pushVar fvs
    return $ push_fvs_code <> [PushInt closure_entries, PushInt n_locals, PushRef body_code_ref, AllocFun arity (length fvs)]

-- Compile a function body using the given arguments and capturing the given
-- free variables.
cgFun :: [Var] -> [Var] -> VExpr -> CM (Instructions, Int, Int)
cgFun args fvs body = do
    (body_code, closure_entries, n_locals) <- withClosure args fvs (cgExpr body)
    return (body_code, closure_entries, n_locals)

cgLet :: Bind Var -> VExpr -> CM Instructions
cgLet bind body = do
    bind_code <- cgBind False bind
    (bind_code <>) <$> cgExpr body

cgMatch :: VExpr -> [Alt Var] -> CM Instructions
cgMatch scrut alts = do
    scrut_code <- cgExpr scrut
    -- TODO: This can be done a *lot* better but we just do linear scan on tags/lits basically.
    (scrut_code <>) <$> cgAlts alts

-- Assumes the evaluated scrutinee is on the top of the stack
cgAlts :: [Alt Var] -> CM Instructions
cgAlts [] = pure []
cgAlts ((WildAlt rhs) : _) = cgExpr rhs
cgAlts ((LitAlt l rhs) : alts) = do
    undefined
cgAlts ((ConAlt con bndrs rhs) : alts) = undefined

cgBind :: Bool -> Bind Var -> CM Instructions
cgBind is_top bind = do
    case bind of
        Bind var rhs ->
            if is_top then cgBndrTop var rhs else cgBndr var rhs
        -- Either a function or value on the stack now
        RecBinds pairs ->
            if is_top
                then mapM_ (uncurry cgBndrTop) pairs >> return top_instrs
                else concatMapM (uncurry cgBndr) pairs
  where
    top_instrs = [error "cgBindTop"]
    cgBndrTop var rhs = do
        case rhs of
            Lam{} -> do
                let (args, body) = collectLamBinders rhs
                (code, _entries, _locals) <- cgFun args [] body
                _ <- addGlobalM (getName var) (VmCode code)
                return top_instrs
            Lit l -> do
                _ <- addGlobalM name $ VmLit l
                return top_instrs
            _ -> do
                error "todo: top expr"
      where
        name = getName var
    cgBndr var rhs = do
        code_rhs <- cgExpr rhs
        -- We could push lets like this on the working stack too I guess
        loc_idx <- addLocalVar var
        pure $ code_rhs <> [SetLocal loc_idx]

mkFunExpr :: VExpr -> VExpr
mkFunExpr e@(Lam{}) = e
mkFunExpr e = Lam (mkFreeVar e) e

cgExpr = undefined
