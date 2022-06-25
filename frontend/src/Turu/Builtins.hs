module Turu.Builtins where

import Turu.Prelude

import Data.HashMap.Strict as M
import Turu.AST
import Turu.AST.Name

builtinUnit :: CompilationUnit Var
builtinUnit = Unit builtinUnitName fake_binds []

renameBuiltin :: Name -> Var
renameBuiltin n
    | Just v <- M.lookup n builtinMapping =
        v
    | otherwise = error $ "Non-existant builtin:" <> show n

builtinMapping :: HashMap Name Var
builtinMapping = M.fromList $ fmap (\v -> (getName v, v)) builtins

fake_binds :: [Bind Var]
fake_binds = fmap mkBuiltinBind builtins

builtins :: [Var]
builtins = [addIntVar]

---

mkBuiltinBind :: Var -> Bind Var
mkBuiltinBind v = Bind v (Var v)

mkBuiltinName :: Text -> Name
mkBuiltinName n = Name n $ Just builtinUnitName

mkBuiltinVar :: Name -> Var
mkBuiltinVar name = MkVar 0 name (VarInfo Nothing)

----
addIntName :: Name
addIntName = mkBuiltinName "addInt"

addIntVar :: Var
addIntVar = mkBuiltinVar addIntName

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
