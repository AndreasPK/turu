{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Turu.AST.Utils where

import Turu.Prelude as P

import Turu.AST
import Turu.AST.Name

import Data.Set as S
import Data.String

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

-----------
freeVarsExpr :: Ord a => Expr a -> Set a
freeVarsExpr e =
    case e of
        Lit{} -> mempty
        Var v -> singleton v
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