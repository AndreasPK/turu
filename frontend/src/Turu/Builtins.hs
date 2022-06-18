module Turu.Builtins where

import Turu.AST

-- Executing a builtin is a transformation of the input state into some output state
type RunBuiltIn state = state -> Expr Var -> (state, Expr Var)

builtinUnit :: UnitName
builtinUnit = UnitName "Builtin"

putChar :: IdInfo
putChar =
    VarInfo
        { info_unique = 1
        , info_name = "putChar"
        , info_impl = Nothing
        , info_unit = builtinUnit
        }

putStr :: IdInfo
putStr =
    VarInfo
        { info_unique = 2
        , info_name = "putStr"
        , info_impl = Nothing
        , info_unit = builtinUnit
        }

putInt :: IdInfo
putInt =
    VarInfo
        { info_unique = 3
        , info_name = "putInt"
        , info_impl = Nothing
        , info_unit = builtinUnit
        }

getInt :: IdInfo
getInt =
    VarInfo
        { info_unique = 4
        , info_name = "getInt"
        , info_impl = Nothing
        , info_unit = builtinUnit
        }

addInt = undefined