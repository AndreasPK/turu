{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Turu.AST where

import Turu.Prelude

import Turu.AST.Name
import Turu.Pretty

import Data.Hashable
import Data.String
import Data.Text
import GHC.TypeLits
import Text.Show.Pretty hiding (Name)

type Unique (domain :: Symbol) = Int
type VUnique = Unique "Var"
type CUnique = Unique "Con"

data VarType
    = ConFam
    | Id

type Tag = Int

{- Constructors and vars
~~~~~~~~~~~~~~~~~~~~~~~~

-}
data Var
    = -- TODO: GHC has a notion of "internal/local" variables. That might be useful in the future.

      -- | something from another unit
      MkVar
      { v_unique :: VUnique
    -- ^ Currently not really used - sadface
      , v_name :: Name
    -- ^ Human name for id, we consider vars with the same name to reference the same thing currently.
      , v_info :: IdInfo
      }
    deriving (Show)

instance Eq Var where
    (==) v1 v2 = v_name v1 == v_name v2

-- instance Show Var where
--     show MkVar { v_unique, v_unit, v_info } =
--         show v_unique <> ":" <> show v_unit <> ":" <> show v_info

instance Printable Var where
    ppr MkVar{v_name} = parens (ppr v_name)

instance HasName Var where
    getName v = v_name v

getVarConArgs :: Var -> Maybe [FamDef Var]
getVarConArgs (MkVar{v_info})
    | FamConInfo{info_con} <- v_info =
        Just $ c_fields info_con
    | otherwise = Nothing

mkValVar :: VUnique -> Text -> UnitName -> Var
mkValVar u n unit = MkVar{v_unique = u, v_name = mkName unit n, v_info = simpValInfo}

simpValInfo :: IdInfo
simpValInfo = VarInfo Nothing

-- | Information about an id shared by all occurences, stored in the symbol table.
data IdInfo
    = VarInfo
        { info_impl :: Maybe (Expr Var)
        -- ^ The rhs if applicable. For now actually unused!
        }
    | -- | Also covers fams
      FamConInfo
        { info_con :: ~DataCon
        }

-- deriving (Show)

instance Show IdInfo where
    show _var@VarInfo{} = "VarInfo{}"
    show _var@FamConInfo{} = "FamConInfo{}"

instance Printable (IdInfo) where
    ppr = ppDoc

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

instance Printable (DataCon) where
    ppr = ppDoc

-- Should we have a con app case in Expr?

-- | Initially a identifier can just be a string so we parametrize
data Expr identifier
    = Lit Literal
    | Var identifier
    | App (Expr identifier) [Expr identifier]
    | Lam identifier (Expr identifier)
    | Let (Bind identifier) (Expr identifier)
    | Match {e_scrut :: identifier, e_alts :: [Alt identifier]}
    deriving (Show, Eq)

type VExpr = Expr Var
instance (Printable a, Show a) => Printable (Expr a) where
    ppr = ppDoc

data Alt identifier
    = LitAlt Literal (Expr identifier)
    | ConAlt identifier [identifier] (Expr identifier)
    | WildAlt (Expr identifier) -- `seq`
    deriving (Show, Eq)

instance (Printable a, Show a) => Printable (Alt a) where
    ppr = ppDoc

data Literal
    = LString Text
    | LitInt Int
    | LitChar Char
    deriving (Show, Eq)

instance Printable Literal where
    ppr (LString t) = doubleQuotes $ ppr t
    ppr (LitInt n) = ppr n
    ppr (LitChar c) = quotes $ char c

data FamDef identifier = FamDef {fd_var :: ~identifier, fd_cons :: ~[ConDef identifier]}
    deriving (Eq, Show)

instance (Printable a, Show a) => Printable (FamDef a) where
    ppr = ppDoc

getFamCons :: FamDef i -> [ConDef i]
getFamCons = fd_cons

data ConDef identifier = ConDef {cd_var :: ~identifier, cd_tag :: Tag, cd_args :: ~[identifier]}
    deriving (Eq, Show)

instance (Printable a, Show a) => Printable (ConDef a) where
    ppr = ppDoc

instance HasName a => HasName (ConDef a) where
    getName = getName . cd_var

data Bind identifier
    = Bind identifier (Expr identifier)
    | RecBinds [(identifier, (Expr identifier))]
    deriving (Show, Eq)

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
