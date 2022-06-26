{-# LANGUAGE InstanceSigs #-}

module Turu.Eval.Builtins where

import Turu.Prelude

import Control.Monad.Identity (Identity)
import Data.Char
import qualified Data.Text as T
import Debug.Trace
import GHC.IO (unsafePerformIO)
import Text.Show.Pretty (ppShow)

import Turu.AST
import Turu.AST.Name (HasName (..), Name, isBuiltinName)
import Turu.AST.Utils
import Turu.Builtins
import Turu.Builtins.PrimOps (PrimOp (..))
import Turu.Eval.Show (showClosure)
import Turu.Eval.Types
import Turu.Pretty

class Monad m => PrimopMonad m where
    evalBuiltinM :: PrimOp -> [Closure] -> m Closure

instance PrimopMonad EM where
    evalBuiltinM :: PrimOp -> [Closure] -> EM Closure
    evalBuiltinM op args = return $ evalBuiltinUnsafe op args

evalBuiltinUnsafe :: PrimOp -> [Closure] -> Closure
evalBuiltinUnsafe op args =
    case op of
        AddInt -> addInt args
        Print -> unsafePerformIO $ do
            putStrLn $ render $ hcat $ fmap (showClosure False) args
            return $ Obj (LitInt 0)
        ReadInt -> unsafePerformIO $ do
            i <- readLn :: IO Int
            return $ Obj (LitInt i)
        ReadStr -> unsafePerformIO $ do
            s <- getLine :: IO String
            return $ Obj (LitString $ T.pack s)
        StrLen -> strLen args
        HeadStr -> headStr args
        TailStr -> tailStr args
        ConsStr -> consStr args
        SnocStr -> snocStr args

addInt :: [Closure] -> Closure
addInt [Obj (LitInt x), Obj (LitInt y)] = Obj (LitInt $ x + y)
addInt args = error $ "addInt - wrong argument objects " <> (ppShow args)

strLen :: [Closure] -> Closure
strLen [Obj (LitString s)] = Obj $ LitInt $ T.length s
strLen _ = error "strLen: Invalid Argument"

headStr :: [Closure] -> Closure
headStr [Obj (LitString s)] = Obj $ LitInt $ ord $ T.head s
headStr _ = error "strLen: Invalid Argument"

tailStr :: [Closure] -> Closure
tailStr [Obj (LitString s)] = Obj $ LitString $ T.tail s
tailStr _ = error "strLen: Invalid Argument"

consStr :: [Closure] -> Closure
consStr [Obj (LitInt c), Obj (LitString s)] = Obj $ LitString $ T.singleton (chr c) <> s
consStr _ = error "strLen: Invalid Argument"

snocStr :: [Closure] -> Closure
snocStr [Obj (LitString s), Obj (LitInt c)] = Obj $ LitString $ s <> T.singleton (chr c)
snocStr _ = error "strLen: Invalid Argument"
