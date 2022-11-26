-- dead module RIP

module Turu.SymTab where

import Data.IntMap.Strict
import Turu.AST
import Turu.AST.Var

newtype SymbolTable = SymbolTable (IntMap IdInfo)

class Monad m => WithSymEnvM m where
    getSymbol :: Var -> m IdInfo
    setSymbol :: Var -> m IdInfo
