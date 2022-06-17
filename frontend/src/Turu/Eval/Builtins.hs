module Turu.Eval.Builtins where

import Turu.Prelude

import Debug.Trace
import Turu.AST
import Turu.AST.Utils
import Turu.Builtins
import Turu.Eval.State

-- Function and arguments must already be evaluated
tryBuiltin :: WithHeap m => Expr -> [Expr] -> m (Expr)
tryBuiltin (Var fun) args
    | v_unit fun == builtinUnit =
        -- TODO hash the function name/make it a constructor/a different IdInfo?
        case info_name (v_info fun) of
            "putChar" -> error "TODO"
            "putStr" -> error "TODO"
            "putInt"
                | [Var arg] <- args ->
                    error "TODO"
                | [Lit lit] <- args ->
                    trace ("putInt " ++ show lit) $ return (Lit LitUnit)
            "getInt" -> return $ mkIntE 42
tryBuiltin fun args = error $ "Invalid builtin application" ++ show fun ++ " " ++ show args