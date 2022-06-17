module Turu.AST.Utils where

import Turu.AST

mkIntE :: Int -> Expr
mkIntE n = Lit (LitInt n)