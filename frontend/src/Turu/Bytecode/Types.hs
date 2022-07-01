{-# LANGUAGE MagicHash #-}

module Turu.Bytecode.Types where

import Data.Int
import GHC.Exts
import Turu.AST
import Turu.AST.Name

type Offset = Int

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
