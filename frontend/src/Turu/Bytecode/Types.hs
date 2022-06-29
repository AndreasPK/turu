module Turu.Bytecode.Types where

import Data.Int

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
    | Jump {- s:[target]-}
    | Return
    | -- | Oh oh
      Abort
    | CmpStr -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | CmpInt -- [a,b] -> (a<b:-1, a==b:0, a>b:1)
    | BranchTbl -- TODO
    | BranchEq -- [lbl_otherwise,a,b] a==b: [], a/=b: jmp lbl_otherwise, []