{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Turu.AST.Var where

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
data Var =
    -- TODO: GHC has a notion of "internal/local" variables. That might be useful in the future.

    MkVar
    { v_unique :: VUnique
    -- ^ Currently not really used - sadface
    , v_name :: Name
    -- ^ Human name for id, we consider vars with the same name to reference the same thing currently.
    , v_info :: IdInfo
    , v_ty :: Maybe Type
    }
    deriving (Show)

instance Eq Var where
    (==) v1 v2 = v_name v1 == v_name v2

instance Ord Var where
    {-# INLINE compare #-}
    compare v1 v2 = getName v1 `compare` getName v2

-- instance Show Var where
--     show MkVar { v_unique, v_unit, v_info } =
--         show v_unique <> ":" <> show v_unit <> ":" <> show v_info

instance Printable Var where
    ppr MkVar{v_name,v_ty} = parens (ppr v_name <> pprMaybe v_ty)

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
mkValVar u n unit = MkVar{v_unique = u, v_name = mkName unit n, v_info = simpValInfo, v_ty = Nothing}

simpValInfo :: IdInfo
simpValInfo = VarInfo

-- | Information about an id shared by all occurences, stored in the symbol table.
data IdInfo
    = VarInfo
        { -- info_impl :: Maybe (Expr Var)
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
    | ParsedCon
        {c_name :: Name}

instance Eq DataCon where
    (==) c1 c2 = c_name c1 == c_name c2
instance HasName DataCon where
    getName = c_name
instance Show DataCon where
    show con = show (c_name con) <> "[" <> sort con <> "," <> ppShow (fmap (v_name . fd_var) $ c_fields con) <> "]"
      where
        sort DataCon{} = "D"
        sort FamCon{} = "F"
        sort ParsedCon{} = "?C?"

instance Printable (DataCon) where
    ppr = ppDoc

data BndrType = BName | BVar

type family ConIdentifier a where
    ConIdentifier Name = Var
    ConIdentifier a = DataCon

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
