{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

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

type Unique (domain :: Symbol) = Int
type VUnique = Unique "Var"
type CUnique = Unique "Con"

data VarType
    = ConFam
    | Id

type Tag = Int
type Arity = Int

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

getVarConArgs :: HasCallStack => Var -> [FamDef Var]
getVarConArgs v@(MkVar{v_info})
    | FamConInfo{info_con} <- v_info =
        c_fields info_con
    | otherwise = error $ "Not a con:" <> ppShow v

getVarCon :: Var -> Maybe DataCon
getVarCon var
    | FamConInfo{info_con} <- v_info var =
        Just info_con
    | otherwise = Nothing

getConArity :: DataCon -> Arity
getConArity con = P.length $ c_fields con

conVarArity :: Var -> Int
conVarArity v = maybe (error "conVarArity") getConArity (getVarCon v)

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
    | PrimInfo {info_prim :: PrimOp}

-- deriving (Show)

instance Show IdInfo where
    show _var@VarInfo{} = "VarInfo{}"
    show _var@FamConInfo{} = "FamConInfo{}"
    show _var@PrimInfo{info_prim} = "PrimInfo(" <> show info_prim <> ")"

-- show var@FamConInfo{} = "FamConInfo{info_con = " <> ppShow (info_con var) <> "}"

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

instance Show DataCon where
    show con = show (c_name con) <> "[" <> sort con <> "," <> ppShow (fmap (v_name . fd_var) $ c_fields con) <> "]"
      where
        sort DataCon{} = "D"
        sort FamCon{} = "F"

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
    = LitString Text
    | LitInt Int
    | LitChar Char -- Not used atm
    deriving (Show, Eq)

instance Printable Literal where
    ppr (LitString t) = doubleQuotes $ ppr t
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
