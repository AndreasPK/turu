{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Turu.Tc where

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

import Turu.AST

-- emptyTcEnv ::
-- exprTy :: Expr a -> Type
-- exprTy expr = case expr of
--     Lit{} -> TyVal $ mkTyArity 0
--     Var{} -> TyAny
--     App{} -> TyAny
--     Lam{} -> TyAny
--     Let b e -> exprTy e
--     Match{} -> TyAny