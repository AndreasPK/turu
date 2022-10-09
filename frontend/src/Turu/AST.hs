{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Turu.AST where

import Turu.Prelude as P

import Turu.AST.Name
import Turu.Pretty

import Data.Hashable
import Data.String
import Data.Text
import qualified Data.Text as T
import GHC.TypeLits

import Text.Show.Pretty hiding (Name)
import Turu.Builtins.PrimOps (PrimOp)
import Turu.Tc.Type
import Turu.AST.Var


-- | Initially a identifier can just be a string so we parametrize
data Expr identifier
    = Lit Literal
    | Var identifier
    | App (Expr identifier) [Expr identifier]
    | Lam identifier (Expr identifier)
    | Let (Bind identifier) (Expr identifier)
    | Match {e_scrut :: identifier, e_alts :: [Alt identifier]}
    deriving (Show, Eq, Functor)

type VExpr = Expr Var
instance (Printable a, Show a) => Printable (Expr a) where
    ppr = ppDoc

data BndrType = BName | BVar

data Alt identifier
    = LitAlt Literal (Expr identifier)
    | ConAlt DataCon [identifier] (Expr identifier)
    | WildAlt (Expr identifier) -- `seq`
    deriving (Show, Eq, Functor)

instance (Printable a, Show a) => Printable (Alt a) where
    ppr = ppDoc


data Bind identifier
    = Bind identifier (Expr identifier)
    | RecBinds [(identifier, (Expr identifier))]
    deriving (Show, Eq, Functor)

instance (Printable a, Show a) => Printable (Bind a) where
    ppr (Bind v rhs) = ppr v <> text " = " <> ppDoc rhs
    ppr (RecBinds binds) = vcat $ fmap ppr binds

binderVars :: Bind identifier -> [identifier]
binderVars (Bind b _rhs) = [b]
binderVars (RecBinds bs) = fmap fst bs

data CompilationUnit identifier = Unit {unit_name :: UnitName, unit_binds :: [Bind identifier], unit_fams :: [FamDef identifier]}
    deriving (Eq, Show)

instance (Show a, Printable a) => Printable (CompilationUnit a) where
    ppr (Unit{unit_name, unit_binds}) = ppr unit_name $$ (vcat $ fmap ppr unit_binds)
