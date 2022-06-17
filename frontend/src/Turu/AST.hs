{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Turu.AST where

import Turu.Prelude

import Data.Hashable
import Data.Text
import GHC.TypeLits

type Unique (domain :: Symbol) = Int
type VUnique = Unique "Var"
type CUnique = Unique "Con"

data Var
    = -- TODO: GHC has a notion of "internal/local" variables. That might be useful in the future.

      -- | something from another unit
      External
      { v_unique :: VUnique
      , v_unit :: UnitName
      , v_info :: IdInfo
      }
    deriving (Show)

data DataCon = DataCon
    { c_unique :: CUnique
    , c_tag :: Int
    -- ^ 1 based, because why not
    , c_name :: Text
    , c_fields :: [Text]
    }
    deriving (Show)

-- | Information about an id shared by all occurences, stored in the symbol table.
data IdInfo = Info
    { info_unique :: VUnique
    -- ^ key for this id
    , info_name :: Text
    -- ^ Human name for id
    , info_impl :: Maybe Expr
    -- ^ The rhs if applicable
    , info_unit :: UnitName
    -- ^ Defining unit
    }
    deriving (Show)

data Expr
    = Lit Literal
    | App Expr [Expr]
    | Var Var
    | Lam Var Expr
    | Con DataCon
    | Case {e_scrut :: Expr, e_alts :: [Alt]}
    deriving (Show)

data Alt
    = LitAlt Literal Expr
    | ConAlt DataCon [Var] Expr
    | AnyAlt Expr -- `seq`
    deriving (Show)

data Literal
    = LString Text
    | LitInt Int
    | LitChar Char
    | LitUnit
    deriving (Show)

data Bind = Bind Var Expr
    deriving (Show)

-- | A unit of compilation is identified by it's **name**
newtype UnitName = UnitName {un_name :: Text} deriving (Eq, Hashable, Show)

data CompilationUnit = Unit {unit_name :: Text, unit_binds :: [Bind]}
