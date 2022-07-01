{-# LANGUAGE MagicHash #-}

module Turu.Bytecode.Types where

import Control.Monad.State.Strict
import Data.Int
import GHC.Exts

import Data.IntMap (IntMap)
import Data.Maybe
import Data.Text (Text)
import Turu.AST
import Turu.AST.Name
import Turu.Prelude

data StackUse
    = Bounded Int
    | Unbounded

-- We might chose to embed things like source locations here in the future.
data VmConstant = VmLiteral Literal

data CompilerState = CompilerState
    { cs_stackUse :: !StackUse
    , cs_constants :: IntMap VmConstant
    }

type CM a = State CompilerState a
type Offset = Int
type ConstRef = () -- TODO: This should essentially be a pointer. Not sure how to best model this yet

initCompilerState :: CompilerState
initCompilerState = CompilerState{cs_stackUse = Bounded 0, cs_constants = mempty}

runCM :: Maybe CompilerState -> CM a -> (a, CompilerState)
runCM m_state act = runState act (fromMaybe initCompilerState m_state)

state_addConstant :: CompilerState -> VmConstant -> (CompilerState, Int)
state_addConstant state =
    let constants = cs_constants
     in undefined

-------------------------

data FunRef
    = FunRefText Name Int -- Identifier of function by name, has to be resolved when loading bytecode
    | FunRefPtr (FunPtr ()) -- Not stored on disk, ref replace by a ptr

data StackEntry
    = StackLit Literal Int
    | StackFun FunRef
    | FunPos FunRef Offset

data Instruction
    = -- Primops
      AddInt
    | Print
    | ReadInt
    | ReadStr
    | StrLen
    | HeadStr
    | TailStr
    | ConsStr
    | SnocStr
    | Call !Int8 -- num args, args on stack
    | Tailcall !Int8 -- num arg, args on stack
    | GetArg -- slot on stack
    | GetLocal -- slot on stack
    | SetLocal -- s:[value,slot] -> locals[slot] = vale, s:[]
    | GetClosureVar -- index on stack
    | SetClosureVar -- s:[value,slot]
    | AllocCon -- [tag,args] -> [heap_ref]
    | AllocClosure -- [env,code_ptr] -> [heap_ref]
    | PushInt -- maxBound :: Int24
    | Jump {- s:[target]-}
    | Return
    | -- | Oh oh
      Abort
    | CmpStr -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | CmpInt -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | BranchTbl -- TODO
    | BranchEq -- [lbl_otherwise,a,b] a==b: [], a/=b: jmp lbl_otherwise, []
