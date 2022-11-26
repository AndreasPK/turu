{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Turu.Tc.Type where

import Turu.Prelude as P
import Turu.AST.Name

import Control.Monad
import Data.Map.Strict
import Data.String
import Data.Text as T

import Data.Bifunctor
import Data.List.NonEmpty as NE



data IsSquiggly = NotSquiggly | Squiggly

type TcEnv = ()

data Type
    = FunTy Int -- ^ either 0 or the length of the list
            [Type] -- ^ non-empty
            Type
    | Value -- Value and definitely not a function
    | TyVar Name
    | ForAllTy [Name] Type
    | TopTy
    deriving (Eq,Ord,Show)



instance Printable Type where
    ppr ty = fromString $ show ty

data NameT
    = MkNameT { nameT_name :: Name
              , nameT_ty :: Maybe Type
              }
    deriving (Eq,Show,Ord)

instance HasName NameT where
    getName = nameT_name

-- Makes a untyped names only
instance IsString NameT where
    fromString s = MkNameT (fromString s) Nothing

noTy :: Maybe Type
noTy = Nothing

untypedName :: Name -> NameT
untypedName n = MkNameT n noTy

mkFunTy :: Int -> Maybe [Type] -> Type -> Type
mkFunTy arity (Just argTys) res
    | assert (P.length argTys == arity) "mkFunTy - arg check" True
    = FunTy arity (argTys) res
mkFunTy arity Nothing res = FunTy arity (P.replicate arity TopTy) res

-- | We only know the arity but not the result of applying the function
-- nor the arguments.
mkArityOnlyTy :: Int -> Type
mkArityOnlyTy arity = FunTy arity (P.replicate arity TopTy) TopTy

-- nameT_ty :: NameT -> Maybe Ty
-- nameT_ty n = snd n

-- f b x y =
--     case b of
--         1  -> let big = ... in g big -- :: n ~> n
--         2 -> let big = ... in -> \x -> h big x -- :: n -> n


-- g z x = x
-- h z = \ x -> x

-- f b x y =
--     case b of
--         True  -> x
--         False -> y

-- foo xs = map zipWith xs
-- foo xs = map (\x -> zipWith x) xs
-- f b x y
-- LHS <3, [b,x,y]->r>

-- RHS: case: b ~ Bool
-- case-alts:
-- ty_case ~ foldr1 unify alt_ty
-- alt_ty: x
--         x ~



-- data NameT
--     = MkNameT { nameT_name :: Name
--               , nameT_ty :: Maybe Ty
--               }
--     deriving (Eq,Show,Ord)

-- instance HasName NameT where
--     getName = nameT_name

-- -- Makes a untyped names only
-- instance IsString NameT where
--     fromString s = MkNameT (fromString s) Nothing

-- instance Printable Ty where
--     ppr ty = fromString $ show ty
-- -- nameT_ty :: NameT -> Maybe Ty
-- -- nameT_ty n = snd n
-- noTy :: Maybe Ty
-- noTy = Nothing

-- untypedName :: Name -> NameT
-- untypedName n = MkNameT n noTy

-- mkUntypedName :: UnitName -> Text -> NameT
-- mkUntypedName x y = uncurry MkNameT (mkName x y, noTy)

