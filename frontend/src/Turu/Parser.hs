{-# LANGUAGE NoImplicitPrelude #-}

module Turu.Parser where

import Turu.Prelude as P hiding (lex, takeWhile)

import Turu.AST
import Turu.AST.Utils

import qualified Control.Applicative.Combinators as C
import Control.Monad
import Control.Monad.Trans.State.Strict (runState)
import qualified Control.Monad.Trans.State.Strict as ST
import Data.Char (isAlphaNum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text as T
import Debug.Trace
import Text.Megaparsec as M hiding (match)
import Text.Megaparsec.Char as M hiding (space)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{-
We want to rely on parantheses quite a lot for a start. A fully formed unit would look something like:

unit Fib -- This is a comment - unit <Name> starts a new compilation unit.

fib n = match n of
        [   0 -> (0)
        ,   1 -> (1)
        ,   _ -> (+) (fib (n-1)) (fib (n-2))
        ]

    )

-- For now do the lispy (?) thing of no infix expressions and parens around all expressions.
-- But we probably want to allow non-parens expressions where possible to keep ourselves sane.
-- This is a rough sketch for the grammar with parens everywhere:

lineComment = "--" <text> '\n'
blockComment = '{- <text> -}'

number = [+-][0-9]*

name = [lowerCaseUnicode][alphaNumericUnicode]*

unit = "unit" <name>\n
       binds

defs = bind
     | famDef

famDef = "fam" fam_name "=" consDefs

consDefs
    = con
    | con "|" consDef

con = upperCaseName fam_name

-- args are sugar for lambdas on rhs
bind = <name> arg_list '=' expr

-- Maybe try to allow expr1 instead of expr where possible?
expr = '(' expr1 ')'

expr1 = lit
      | app
      | var
      | lam
      | let
      | match
      | expr
      -- ^ is this ambigous? If we have `f ((` I guess it needs a lot of backtracking to differentiate between
      `(x)` and `((x) (y)). That is because only when we get to y's  opening parens we know for sure if we have
          expr instead of app. Might still worth having.

lit = "text"
    | number  -- only ints perhaps for now?
    | 'c' -- char for some c

var = <name>

app = <list_of_exprs> -- at least two!!

lam = '\' arg_list '->' expr

con = conName list_of_exprs

conName = CapitalLetter:letters (or keyword + lowercase?)

let = 'let' var 'in' expr

match = 'match' var 'of' alts

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
    , pi_binds :: ~[Bind Text]
    , pi_uniq :: Int
    }

initParseInfo = ParseInfo (UnitName "NoUnitName") [] 0

data ErrorInfo = ErrorInfo
    deriving (Eq, Ord, Show) -- For now :(

instance ShowErrorComponent ErrorInfo where
    showErrorComponent (ErrorInfo) = ""
    errorComponentLen (ErrorInfo) = 0

type TParseMonad = ST.State ParseInfo

type P a = ParsecT ErrorInfo Text TParseMonad a
type PState = M.State Text ErrorInfo

initParseState :: Text -> State Text ErrorInfo
initParseState input =
    let posState = PosState input 0 (initialPos "NoFile") (mkPos 0) ""
     in State input 0 posState []

runParserStateM :: Text -> P a -> TParseMonad (PState, Either (ParseErrorBundle Text ErrorInfo) a)
runParserStateM input parser =
    let pstate = initParseState input :: PState
     in runParserT' parser (initParseState input)

runParser :: Text -> P a -> Maybe a
runParser input parser =
    let (parse_result, state') = runState (runParserStateM input parser) initParseInfo
        (parse_state, parsed_val) = parse_result
        remainder = stateInput parse_state
     in traceShow remainder $ either (error . show) Just parsed_val

-- Actual parsers

keywords :: [Text]
keywords = ["match", "in", "let", "fam", "Any"]

lineComment :: P ()
lineComment = L.skipLineComment "--"

blockComment :: P ()
blockComment = L.skipBlockCommentNested "{-" "-}"

-- Consume white space (including newlines)
space :: P ()
space = L.space space1 lineComment blockComment

-- Consume whitespace after thing
lex :: P a -> P a
lex = L.lexeme space

sym :: Text -> P Text
sym = L.symbol space

-- Parse a keyword
key :: Text -> P ()
key word = (string word *> notFollowedBy alphaNumChar) <* space

name :: P Text
name = do
    c <- lowerChar
    r <- many alphaNumChar :: P String
    space
    let n = pack (c : r)
    if n `P.elem` keywords
        then M.failure (Just $ Tokens $ NE.fromList $ T.unpack n) (S.singleton $ Label $ NE.fromList "identifier")
        else return $ n

unitDef :: P UnitName
unitDef =
    UnitName <$> do
        lex (tokens (==) "unit") *> lex name

binds :: P [Bind Text]
binds = many bind

unit :: P (CompilationUnit Text)
unit = do
    n <- unitDef
    bs <- binds
    return $ Unit n bs

number :: P Int
number = lex $ L.signed mempty L.decimal

pleft :: P ()
pleft = void $ L.symbol space "("

pright :: ParsecT ErrorInfo Text TParseMonad ()
pright = void $ L.symbol space ")"

aright :: P ()
aright = void $ sym "->"

famDef :: P (FamDef Text)
famDef = do
    key "fam"
    fam_name <- conName
    sym "="
    con_defs <- consList 0
    return $ FamDef fam_name con_defs

-- True | False | Either
consList :: Int -> P [ConDef Text]
consList tag = do
    con1 <- consDef tag
    cons <- try (sym "|" *> (consList (tag + 1) <|> pure []))
    return $ con1 : cons

consDef :: Int -> P (ConDef Text)
consDef tag = do
    con_name <- conName
    con_args <- many conName
    return $ ConDef con_name tag con_args

bind :: P (Bind Text)
bind = do
    n <- name
    args <- many name
    _ <- sym "="
    e <- expr
    return $ Bind n (mkLams args e)

type ExprP = P (Expr Text)

-- expression without parens
expr :: ExprP
expr = try (pleft *> expr1 <* pright) <|> try var <|> lit <|> try letp <|> try match

-- ( expr )
expr1 :: ExprP
expr1 = try app <|> try lam <|> expr

lit :: ExprP
lit = Lit <$> lit1

lit1 :: P Literal
lit1 = (slit <|> nlit <|> clit) <* space

slit, nlit, clit :: P Literal
slit = LString <$> between (char '"') (char '"') (takeWhileP (Just "String") (/= '"'))
nlit = LitInt <$> number
clit = LitChar <$> between (char '\'') (char '\'') anySingle

-- unitlit = sym "()" *> pure LitUnit

letp :: ExprP
letp = do
    key "let"
    n <- name
    args <- many name
    sym "="
    rhs <- expr
    key "in"
    body <- expr
    return $ Let (Bind n $ mkLams args rhs) body

conName :: P Text
conName = do
    c <- upperChar
    cs <- lex $ takeWhileP Nothing (isAlphaNum)
    let conName = c `cons` cs
    return $! conName

lam :: ExprP
lam = Lam <$> (sym "\"" *> name) <*> (aright *> expr)

var :: ExprP
var = Var <$> name

-- In order to make up work without parens we need to make it right(?)-recursive. I can't be bothered atm.
app :: ExprP
app = do
    f <- expr
    args <- some expr
    return $ App f args

match :: ExprP
match = Match <$> (key "match" *> name) <*> match_body

match_body :: P [Alt Text]
match_body = between (sym "[") (sym "]") alts

alts :: P [Alt Text]
alts = sepBy1 (alt) (sym ",")

type AltP = P (Alt Text)
alt :: AltP
alt = litAlt <|> conAlt <|> wildAlt

litAlt, conAlt, wildAlt :: AltP
litAlt = LitAlt <$> (lit1 <* aright) <*> expr
conAlt = ConAlt <$> conName <*> many name <*> (aright *> expr)
wildAlt = WildAlt <$> (sym "_" *> aright *> expr)

-- name :: P Text
-- name = do
--     c <- lowerChar :: P Char
--     r <- many alphaNumChar :: P Text
--     return $ c <> r
