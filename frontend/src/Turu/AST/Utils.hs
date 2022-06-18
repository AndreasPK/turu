{-# LANGUAGE FlexibleInstances #-}

module Turu.AST.Utils where

import Turu.Prelude

import Data.String
import Turu.AST

mkIntE :: Int -> Expr a
mkIntE n = Lit (LitInt n)

mkLams :: [a] -> Expr a -> Expr a
mkLams args rhs = foldr Lam rhs args

instance IsString (Expr Text) where
    fromString s = Var $ pack s