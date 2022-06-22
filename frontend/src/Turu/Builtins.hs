module Turu.Builtins where

import Turu.Prelude

import Turu.AST
import Turu.AST.Name

-- Executing a builtin is a transformation of the input state into some output state
type RunBuiltIn state = state -> Expr Var -> (state, Expr Var)

builtinUnit :: UnitName
builtinUnit = UnitName "Builtin"

mkBuiltinName :: Text -> Name
mkBuiltinName n = Name n $ Just builtinUnit

-- putChar :: IdInfo
-- putChar =
--     VarInfo
--         { info_unique = 1
--         , info_name = mkBuiltinName "putChar"
--         , info_impl = Nothing
--         }

-- putStr :: IdInfo
-- putStr =
--     VarInfo
--         { info_unique = 2
--         , info_name = mkBuiltinName "putStr"
--         , info_impl = Nothing
--         }

-- putInt :: IdInfo
-- putInt =
--     VarInfo
--         { info_unique = 3
--         , info_name = mkBuiltinName "putInt"
--         , info_impl = Nothing
--         }

-- getInt :: IdInfo
-- getInt =
--     VarInfo
--         { info_unique = 4
--         , info_name = mkBuiltinName "getInt"
--         , info_impl = Nothing
--         }
