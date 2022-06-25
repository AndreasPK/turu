module Turu.Eval.Builtins where

import Turu.Prelude

import Debug.Trace
import Text.Show.Pretty (ppShow)
import Turu.AST
import Turu.AST.Name (HasName (..), Name, isBuiltinName)
import Turu.AST.Utils
import Turu.Builtins
import Turu.Eval.Types

evalBuiltin :: Name -> [Closure] -> Closure
evalBuiltin name args =
    assert (isBuiltinName name) "evalBuiltin" $ case name of
        v_name | addIntName == v_name -> addInt args
        _ -> error $ "Failed to evaluate builtin: " <> ppShow name <> " " <> ppShow args

addInt :: [Closure] -> Closure
addInt [Obj (LitInt x), Obj (LitInt y)] = Obj (LitInt $ x + y)
addInt args = error $ "addInt - wrong argument objects " <> (ppShow args)
