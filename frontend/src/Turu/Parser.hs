module Turu.Parser where

import Turu.Prelude

import qualified Control.Monad.Trans.State.Strict as ST
import Text.Megaparsec
import Turu.AST

{-
We want to rely on parantheses quite a lot for a start. A fully formed unit would look something like:

unit Fib -- This is a comment - unit <Name> starts a new compilation unit.

fib n = case n of
        [   0 -> 0
        ,   1 -> 1
        ,   _ -> + (fib (n-1)) (fib (n-2))
        ]

    )

-- For now do the lispy (?) thing of no infix expressions.
-- This is a rough sketch for the grammar than

unit = "unit" <name>\n
       binds

binds = comment
      | emptyLine
      | bind

-- args are sugar for lambdas on rhs
bind = <name> arg_list '=' expr

-- Maybe try to allow expr1 instead of expr where possible?
expr = '(' expr1 ')'

expr1 = lit
      | app
      | <name> -- var
      | lam
      | conName
      | case

lit = "text"
    | number  -- only ints perhaps for now?
    | 'c' -- char for some c
    | '()' -- unit

app = <list_of_exprs> -- at least two!!

lam = '\' arg_list '->' expr

conName = CapitalLetter:letters (or keyword + lowercase?)

case = 'case' expr 'of' '(' alts ')'

alts = '[' alts1 ']'

-- Again a case where i'm not sure if it translates directly to combinators like this
alts1 = alt ',' alts1
      | alt

alt = lit '->' expr
    | conName '->' expr
    | '_' -> expr

-}

data ParseInfo = ParseInfo
    { pi_unit :: ~UnitName
    , pi_binds :: ~[Bind]
    , pi_uniq :: Int
    }

type ErrorInfo = () -- For now :(

type TParseMonad = ST.State ParseInfo

type TParser a = ParsecT ErrorInfo Text TParseMonad a
