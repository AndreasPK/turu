{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Turu.Tc.Type where

import Turu.Prelude as P
import Turu.AST.Name

import Control.Monad
import Data.Map.Strict
import Data.String
import Data.Text as T

-- | Mono-arity types
data Ty = TyArity TyArity | TyVar Text | FunTy [Ty] Ty
    deriving (Show,Eq,Ord)
data TyArity = FixedArity Int | AnyArity
    deriving (Show,Eq,Ord)

data PolyTy = Poly [Text] Ty
    deriving (Show,Eq,Ord)

data TyEnv = TyEnv { te_map :: Map Text PolyTy}

data NameT
    = MkNameT { nameT_name :: Name
              , nameT_ty :: Maybe Ty
              }
    deriving (Eq,Show,Ord)

instance HasName NameT where
    getName = nameT_name

-- Makes a untyped names only
instance IsString NameT where
    fromString s = MkNameT (fromString s) Nothing

instance Printable Ty where
    ppr ty = fromString $ show ty
-- nameT_ty :: NameT -> Maybe Ty
-- nameT_ty n = snd n
noTy :: Maybe Ty
noTy = Nothing

untypedName :: Name -> NameT
untypedName n = MkNameT n noTy

mkUntypedName :: UnitName -> Text -> NameT
mkUntypedName x y = uncurry MkNameT (mkName x y, noTy)

