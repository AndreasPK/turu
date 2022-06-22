module Turu.Eval.Builtins where

import Turu.Prelude

import Debug.Trace
import Turu.AST
import Turu.AST.Utils
import Turu.Builtins
import Turu.Eval.State

-- Function and arguments must already be evaluated
-- tryBuiltin :: WithHeap m => Expr Var -> [Expr Var] -> m (Expr Var)
-- tryBuiltin (Var fun) args
--     | v_unit fun == builtinUnit =
--         -- TODO hash the function name/make it a constructor/a different IdInfo?
--         case info_name (v_info fun) of
--             "putChar" -> error "TODO"
--             "putStr" -> error "TODO"
--             "putInt"
--                 | [Var _arg] <- args ->
--                     error "TODO"
--                 | [Lit lit] <- args ->
--                     trace ("putInt " ++ show lit) $ return (Lit $ LitInt 42)
--             "getInt" -> return $ mkIntE 42
--             _ -> error "NoBuiltin"
-- tryBuiltin fun args = error $ "Invalid builtin application" ++ show fun ++ " " ++ show args