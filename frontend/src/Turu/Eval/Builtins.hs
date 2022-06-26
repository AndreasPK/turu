module Turu.Eval.Builtins where

import Turu.Prelude

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

evalBuiltin :: PrimOp -> [Closure] -> Closure
evalBuiltin op args =
    case op of
        AddInt -> addInt args
        Print -> unsafePerformIO $ do
            putStrLn $ render $ hcat $ fmap (showClosure False) args
            return $ Obj (LitInt 0)
        ReadInt -> undefined
        ReadStr -> undefined
        StrLen -> undefined
        HeadStr -> undefined
        TailStr -> undefined
        ConsStr -> undefined
        SnocStr -> undefined

addInt :: [Closure] -> Closure
addInt [Obj (LitInt x), Obj (LitInt y)] = Obj (LitInt $ x + y)
addInt args = error $ "addInt - wrong argument objects " <> (ppShow args)
