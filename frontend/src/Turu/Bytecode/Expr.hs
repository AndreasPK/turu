module Turu.Bytecode.Expr where

import Turu.AST (Literal (LitString))
import Turu.Bytecode.Types

cgLit :: Literal -> CM [Instruction]
cgLit (LitString s) =
    -- TODO: We have to emit the string constant in what in assembly would be the data
    -- section. Not sure how we want to model this yet.
    undefined