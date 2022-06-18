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

data VarType
    = ConFam
    | Id

type Name = Text
type Tag = Int

data Var
    = -- TODO: GHC has a notion of "internal/local" variables. That might be useful in the future.

      -- | something from another unit
      MkVar
      { v_unique :: VUnique
      , v_unit :: UnitName
      , v_info :: IdInfo
      }
    deriving (Show)

-- | Information about an id shared by all occurences, stored in the symbol table.
data IdInfo
    = VarInfo
        { info_unique :: VUnique
        -- ^ key for this id
        , info_name :: Name
        -- ^ Human name for id
        , info_impl :: Maybe (Expr Var)
        -- ^ The rhs if applicable
        , info_unit :: UnitName
        -- ^ Defining unit
        }
    | -- | Also covers fams
      FamConInfo
        { info_unique :: VUnique
        -- ^ key for this id
        , info_name :: Name
        -- ^ Human name for id
        , info_con :: DataCon
        }
    deriving (Show)

data DataCon
    = DataCon
        { c_unique :: CUnique
        , c_tag :: Tag
        -- ^ 1 based, because why not
        , c_name :: Name
        , c_fields :: ~[FamDef Var]
        }
    | FamCon
        { c_unique :: CUnique
        , c_tag :: Tag
        -- ^ 1 based, because why not
        , c_name :: Name
        , c_fields :: ~[FamDef Var]
        -- ^ Always empty I guess
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

data FamDef identifier = FamDef {fd_var :: ~identifier, fd_cons :: ~[ConDef identifier]}
    deriving (Eq, Show)

data ConDef identifier = ConDef {cd_var :: ~identifier, cd_tag :: Tag, cd_args :: ~[identifier]}
    deriving (Eq, Show)

data Bind identifier = Bind identifier (Expr identifier)
    deriving (Show, Eq)

-- | A unit of compilation is identified by it's **name**
newtype UnitName = UnitName {un_name :: Text} deriving (Eq, Hashable, Show, IsString)

data CompilationUnit identifier = Unit {unit_name :: UnitName, unit_binds :: [Bind identifier]}
    deriving (Eq, Show)
