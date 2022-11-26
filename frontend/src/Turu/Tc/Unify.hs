{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Turu.Tc.Unify where

import Turu.Prelude as P
import Turu.AST.Name
import Turu.Tc.Type

import Control.Monad
import Data.Map.Strict
import Data.String
import Data.Text as T

import Data.Bifunctor
import Data.List.NonEmpty as NE

data UnifyResult a
    = Unify --Subst
    | Unified a
    | MaybeApart
    | SurelyApart { uf_fail :: Text }
    deriving (Show)

type UnifyTypeResult = UnifyResult Type

unify :: TcEnv -> Type -> Type -> UnifyTypeResult
unify _ a b
    -- I guess syntactic equality might not work for foralls
    | a == b = Unified a
unify _ TopTy _ = Unified TopTy
unify _ _ TopTy = Unified TopTy

unify _ Value (FunTy {}) = SurelyApart ""
unify _ (FunTy {}) Value = SurelyApart ""

unify _ ForAllTy{} _ = error "Forall/TyVar TODO"
unify _ _ ForAllTy{} = error "Forall/TyVar TODO"
unify _ TyVar{} _ = error "Forall/TyVar TODO"
unify _ _ TyVar{} = error "Forall/TyVar TODO"

unify env (FunTy arr1 args1 res1) (FunTy arr2 args2 res2)
    | arr1 /= arr2 = SurelyApart "Function arity missmatch"
    | otherwise = unifyFun env (args1,res1) (args2,res2)

unifyFun :: TcEnv -> ([Type],Type) -> ([Type],Type) -> UnifyTypeResult
unifyFun env (args1,res1) (args2,res2) =
    case unify env res1 res2 of
        Unified res ->
            case unifyList env args1 args2 of
                Unified args -> Unified $ mkFunTy (P.length args) (Just args) res
                Unify -> error "TODO"
                SurelyApart r -> SurelyApart r
                MaybeApart -> error "TODO"
        MaybeApart -> error "TODO"
        failed@SurelyApart{} -> failed
        Unify{} -> error "TODO"
    where

unifyList :: TcEnv -> [Type] -> [Type] -> UnifyResult [Type]
unifyList env xs ys
    | P.all isUnified un_args
    = Unified $ fmap unUnify un_args
    | otherwise
    = SurelyApart $ P.foldl' failReason "" un_args
    where
        un_args = P.zipWith (unify env) xs ys

        unUnify (Unified res) = res

        isUnified (Unified{}) = True
        isUnified (Unify{}) = error "TODO - Unify arg list"
        isUnified _ = error "False"

        failReason reasons (Unified{}) = reasons
        failReason reasons (Unify{}) = reasons <> ";Unify not implemented"
        failReason reasons (SurelyApart reason) = reasons <> ";" <> reason
        failReason reasons (MaybeApart{}) = reasons <> ";MaybeApart not implemented"





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

