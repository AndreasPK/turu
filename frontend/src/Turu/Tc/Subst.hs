{-# LANGUAGE MultiParamTypeClasses #-}
module Turu.Tc.Subst where

import Turu.Prelude
import Turu.AST

import Data.Map.Strict as M

newtype Subst a b = Subst (Map a b)

class Substy a b where
    subst :: Subst a b -> a -> b



