module Turu.Eval.State where

import Turu.Prelude

import Turu.AST

import Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{-  What do we need for interpretation?

\* A list of units loaded
\* A way to execute builtins
\* A representation of the heap

-}

type EvalExpr = Var
data Heap = Heap (IM.IntMap EvalExpr)

newtype Ptr = Ptr Int deriving (Eq, Ord)

-- We are lazy so expr = closure here
-- Should be fine for a strict lang?
type Closure = Expr

allocateClosure :: Heap -> EvalExpr -> (Heap, Ptr)
allocateClosure (Heap m) expr =
    let max_ptr
            | IM.null m = 0
            | otherwise = fst (IM.findMax m)
        new_ptr = max_ptr + 1
     in (Heap $! IM.insert new_ptr expr m, Ptr new_ptr)

getClosure :: Heap -> Ptr -> EvalExpr
getClosure (Heap m) (Ptr p) =
    fromMaybe (error "Invalid ptr - urkh") (IM.lookup p m)

mkHeap :: Heap
mkHeap = Heap mempty

newtype HeapM a = HeapM (State Heap a) deriving (Functor, Applicative, Monad)

class Monad m => WithHeap m where
    alloc :: EvalExpr -> m Ptr
    deref :: Ptr -> m EvalExpr

instance WithHeap (HeapM) where
    alloc expr = HeapM $ do
        h <- get
        let (h', ptr) = allocateClosure h expr
        put h'
        return ptr
    deref (p) = HeapM $ do
        h <- get
        let clo = getClosure h p
        return clo
