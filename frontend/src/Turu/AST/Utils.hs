{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Turu.AST.Utils where

import Turu.Prelude as P

import Turu.AST
import Turu.AST.Name
import Turu.AST.Var
import Turu.Tc.Type

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT (hexadecimal)

data Box a = Box ~a

mkIntE :: Int -> Expr a
mkIntE n = Lit (LitInt n)

-- Wrap the expression in lambdas
mkLams :: [a] -> Expr a -> Expr a
mkLams args rhs = P.foldr Lam rhs args

mkLamApp :: [a] -> Expr a -> Expr a
mkLamApp args body = mkLams args (mkApp body $ fmap Var args)

mkApp :: Expr a -> [Expr a] -> Expr a
mkApp = App

-- (\x -> \y -> rhs) == ([x,y], rhs)
collectLamBinders :: Expr a -> ([a], Expr a)
collectLamBinders e = go [] e
  where
    go bndrs (Lam b rhs) = go (b : bndrs) rhs
    go bndrs rhs = (reverse bndrs, rhs)

-- Only useable if there is no unit
instance IsString (Expr Name) where
    fromString s = Var $ Name (fromString s) Nothing

instance IsString (Expr NameT) where
    fromString s = Var $ untypedName $ Name (fromString s) Nothing



-----------
{-# INLINE freeVarsExprWithoutStatic #-}
freeVarsExprWithoutStatic :: (Ord a) => (a -> Bool) -> Expr a -> Set a
freeVarsExprWithoutStatic is_static expr =
    S.filter (not . is_static) $ freeVarsExpr expr

freeVarsExpr :: Ord a => Expr a -> Set a
freeVarsExpr e =
    case e of
        Lit{} -> mempty
        Var v -> S.singleton v
        App f args -> S.unions (freeVarsExpr f : fmap freeVarsExpr args)
        Lam b rhs -> S.delete b $! freeVarsExpr rhs
        Let b body -> freeVarsBind b <> deletes (freeVarsExpr body) (binderVars b)
        Match v alts -> S.singleton v <> mconcat (fmap altFvs alts)

altFvs :: Ord i => Alt i -> Set i
altFvs (WildAlt rhs) = freeVarsExpr rhs
altFvs (LitAlt _l rhs) = freeVarsExpr rhs
altFvs (ConAlt _c bndrs rhs) = deletes (freeVarsExpr rhs) bndrs

freeVarsBind :: Ord a => Bind a -> Set a
freeVarsBind (Bind b rhs) = S.delete b $ freeVarsExpr rhs
freeVarsBind (RecBinds prs) = deletes (mconcat $ fmap freeVarsExpr $ fmap snd prs) (fmap fst prs)

deletes :: Ord i => Set i -> [i] -> Set i
deletes = P.foldl' (flip S.delete)

mkArgVarInfo :: Name -> IdInfo
mkArgVarInfo name
    -- Constructor
    | isUpper (T.head $ n_name name) =
        error "Constructor args not supported"
    | otherwise =
        VarInfo

mkArgVar :: Name -> Var
mkArgVar name =
    let info = mkArgVarInfo name
        var = MkVar 0 name info noTy
     in var

-- Return a value variable that's not free in the given expression
mkFreeVars :: Int -> VExpr -> [Var]
mkFreeVars n expr =
    let fvs = freeVarsExpr expr :: S.Set Var
        mk_txt = LT.toStrict . LT.toLazyText . LT.hexadecimal
        names = fmap (mkLocalName . mk_txt) [1 :: Int ..] :: [Name]
        candidates = fmap mkArgVar names :: [Var]
     in take n $ filter (\v -> notElem v fvs) candidates

mkFreeVar :: VExpr -> Var
mkFreeVar = head . mkFreeVars 1