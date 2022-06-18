{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Turu.AST where

import Turu.Prelude

import Data.Hashable
import Data.String
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
data IdInfo
    = VarInfo
        { info_unique :: VUnique
        -- ^ key for this id
        , info_name :: Text
        -- ^ Human name for id
        , info_impl :: Maybe (Expr Var)
        -- ^ The rhs if applicable
        , info_unit :: UnitName
        -- ^ Defining unit
        }
    | ConInfo
        { info_unique :: VUnique
        -- ^ key for this id
        , info_name :: Text
        -- ^ Human name for id
        , info_con :: DataCon
        }
    deriving (Show)

-- | Initially a identifier can just be a string
data Expr identifier
    = Lit Literal
    | App (Expr identifier) [Expr identifier]
    | Var identifier
    | Lam identifier (Expr identifier)
    | Let (Bind identifier) (Expr identifier)
    | Match {e_scrut :: identifier, e_alts :: [Alt identifier]}
    deriving (Show, Eq)

data Alt identifier
    = LitAlt Literal (Expr identifier)
    | ConAlt identifier [identifier] (Expr identifier)
    | WildAlt (Expr identifier) -- `seq`
    deriving (Show, Eq)

data Literal
    = LString Text
    | LitInt Int
    | LitChar Char
    deriving (Show, Eq)

data Bind identifier = Bind identifier (Expr identifier)
    deriving (Show, Eq)

-- | A unit of compilation is identified by it's **name**
newtype UnitName = UnitName {un_name :: Text} deriving (Eq, Hashable, Show, IsString)

data CompilationUnit identifier = Unit {unit_name :: UnitName, unit_binds :: [Bind identifier]}
    deriving (Eq, Show)
