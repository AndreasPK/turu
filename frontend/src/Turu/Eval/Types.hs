{-# LANGUAGE MultiParamTypeClasses #-}

module Turu.Eval.Types where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Bits (Bits (xor))
import Data.Char
import Data.Coerce
import Data.Map.Lazy as M hiding (update)
import Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.Builtins.PrimOps (PrimOp)
import Turu.Prelude as P
import Turu.Pretty

type Code = Expr Var

type Data = Map Name Closure

data Closure
    = FunClosure {closure_code :: Code, closure_data :: ~Data}
    | Fun {closure_code :: Code}
    | -- | Builtins must all either be primops, or inlined during renamed currently
      Builtin PrimOp
    | Ind Name
    | -- | Objects have no fvs
      Obj Literal
    | -- | a fully applied constructor
      SatCon
        Var
        -- ^ The constructor being applied
        [Closure]
        -- ^ The arguments to the constructor
    deriving (Eq, Show)