{-# LANGUAGE DeriveDataTypeable #-}

module Turu.Builtins where

import Turu.Prelude

import Control.Applicative
import Data.Char (toLower)
import qualified Data.Data as D
import Data.HashMap.Strict as M
import qualified Data.Text as T
import Turu.AST
import Turu.AST.Name
import Turu.AST.Var
import Turu.Tc.Type
import Turu.Builtins.PrimOps

-- Builtin any family
anyFam :: FamDef Var
anyFam = FamDef{fd_var = anyFamV, fd_cons = []}

anyFamV :: Var
anyFamV = MkVar 0 anyFamName anyFamInfo noTy

anyFamName :: Name
anyFamName = mkBuiltinName "Any"

anyFamInfo :: IdInfo
anyFamInfo = FamConInfo anyCon

anyCon :: DataCon
anyCon = FamCon 0 1 (anyFamName) []

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
builtins = fmap mkBuiltinVar builtinNames

builtinNames :: [Name]
builtinNames = primopNames

varPrimOp :: Var -> Maybe PrimOp
varPrimOp var
    | PrimInfo op <- v_info $ var =
        Just op
    | otherwise = Nothing

---

mkBuiltinBind :: Var -> Bind Var
mkBuiltinBind v = Bind v (Var v)

mkBuiltinVar :: Name -> Var
mkBuiltinVar name = MkVar 0 name (fromMaybe (VarInfo) (PrimInfo <$> namePrimOp name)) noTy
