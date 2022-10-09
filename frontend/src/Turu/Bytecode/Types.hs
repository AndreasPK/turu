{-# LANGUAGE MagicHash #-}

-- | We generally try to put thins that are relevant to the object code format here.
module Turu.Bytecode.Types where

import Control.Monad.State.Strict hiding (state)
import Data.Int
import GHC.Exts

import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Var
import Turu.Prelude

data StackUse
    = Bounded Int
    | Unbounded

-- We might chose to embed things like source locations here in the future.
data VmConstant = VmLit Literal | VmCode [Instruction] | VmConApp DataCon [DataRef] deriving (Show)

type Offset = Int
type NameRef = Name -- TODO: This should essentially be a pointer. Not sure how to best model this yet
data DataRef
    = NameRef Name
    | AnonRef Int
    | -- | Relative to the *top* of the stack, so does change when pushing
      StackRefTop Int
    deriving (Show)

-------------------------

data FunRef
    = FunRefText Name Int -- Identifier of function by name, has to be resolved when loading bytecode
    | FunRefPtr (FunPtr ()) -- Not stored on disk, ref replace by a ptr

data StackEntry
    = StackLit Literal Int
    | StackFun FunRef
    | FunPos FunRef Offset

-- Currently my plan is to encode every instruction as a 64-bit word. With 8 bits representing the
-- op code and 56 bit being available as argument.
-- This is nice, since it means we can use the argument to store a pointer. This works since systems
-- currently really only use 48 bits of address space. But we obviously have to zero it before dereferencing
-- anything.
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
    | Call {call_args_given :: !Int} -- num args : s:[fun_closure,arg1,...,argn] -> s:[result] (result could be a pap)
    | TailCall !Int -- num arg, args on stack
    | GetArg Int
    | GetLocal Int
    | SetLocal Int
    | GetClosureVar Int
    | GetClosureVarStack -- index on stack
    | SetClosureVar Int
    | SetClosureVarStack -- s:[value,slot]
    | -- | s:[heap_ref] -> [f1,f2,f3...] extract the fields of a data constructor onto the stack
      PushConFields Int -- Number of fields as argument
    | PushConTag -- s:[heap_ref] -> [tag(boxed)]
    | AllocCon {-the tag-} Int {-number of args-} Int -- [args] -> [heap_ref]
    | -- This way of allocating a function closure is probably highly inefficient
      AllocFun {-arity-} Int {-n_fvs-} Int -- [fv1,fv2,..,fv_n,n_locals,code_ptr] -> [heap_ref]
    | PushInt Int -- maxBound :: Int56
    | PushRef DataRef
    | Jump {- s:[target]-}
    | Return
    | Abort -- oh oh
    | CmpStr -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | CmpInt -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | BranchTbl -- TODO
    | BranchEq -- [lbl_otherwise,a,b] a==b: [], a/=b: jmp lbl_otherwise, []
    deriving (Show)

instrStackDiff :: Instruction -> Int
instrStackDiff i = case i of
    AddInt -> -1
    Print -> -1
    ReadInt -> 1
    ReadStr -> 1
    StrLen -> 0
    HeadStr -> 1
    TailStr -> 1
    ConsStr -> 1
    SnocStr -> 1
    Call _args -> todo
    TailCall _args -> todo
    GetArg{} -> 1
    GetLocal{} -> 1
    SetLocal{} -> -1
    GetClosureVar{} -> 1
    GetClosureVarStack{} -> 0
    SetClosureVar{} -> -1
    SetClosureVarStack{} -> -2
    AllocCon _ n_args -> -n_args + 1
    AllocFun _ fvs -> -fvs - 2 + 1 -- - fvs,locals,code + fun
    PushInt _ -> 1
    PushRef _ -> 1
    Jump -> 0
    Return -> todo
    Abort -> todo
    CmpStr -> -1
    CmpInt -> -1
    BranchTbl -> todo
    BranchEq -> -3
    PushConFields n -> n - 1
    PushConTag -> 0
  where
    todo = error $ "Stack tracking for " <> show i <> " not implemented"