{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Turu.AST.Utils where

import Turu.Prelude

import Data.String
import Turu.AST
import Turu.AST.Name

mkIntE :: Int -> Expr a
mkIntE n = Lit (LitInt n)

mkLams :: [a] -> Expr a -> Expr a
mkLams args rhs = foldr Lam rhs args

-- Only useable if there is no unit
instance IsString (Expr Name) where
    fromString s = Var $ Name (fromString s) Nothing