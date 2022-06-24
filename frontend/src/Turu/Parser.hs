{-# LANGUAGE NoImplicitPrelude #-}

module Turu.Parser where

import qualified Control.Applicative.Combinators as C
import Control.Monad
import qualified Control.Monad.State.Strict as MTLS
import Control.Monad.Trans.State.Strict (runState)
import qualified Control.Monad.Trans.State.Strict as ST
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import Text.Megaparsec as M hiding (match)
import Text.Megaparsec.Char as M hiding (space)
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug as D
import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.Prelude as P hiding (lex, takeWhile)

{-
We want to rely on parantheses quite a lot for a start. A fully formed unit would look something like:

unit Fib -- This is a comment - unit <Name> starts a new compilation unit.

let rec
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

name = [lowerCaseUnicode][alphaNumericUnicode|underscore]*

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
bind = rec many bind1
     | bind1

bind1 = let <name> arg_list '=' expr

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

con = conNameP list_of_exprs

conNameP = CapitalLetter:letters (or keyword + lowercase?)

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
  { pi_unit :: (Maybe UnitName),
    pi_binds :: ~[Bind Text],
    pi_uniq :: Int
  }

initParseInfo :: ParseInfo
initParseInfo = ParseInfo (Nothing) [] 0

data ErrorInfo = ErrorInfo
  deriving (Eq, Ord, Show) -- For now :(

instance ShowErrorComponent ErrorInfo where
  showErrorComponent (ErrorInfo) = ""
  errorComponentLen (ErrorInfo) = 0

type TParseMonad = ST.State ParseInfo

getUnit :: P (Maybe UnitName)
getUnit = do
  s <- MTLS.get
  return $ pi_unit s

setUnit :: Maybe UnitName -> P ()
setUnit unit1 = do
  s <- MTLS.get
  MTLS.put $ s {pi_unit = unit1}

type P a = ParsecT ErrorInfo Text TParseMonad a

type PState = M.State Text ErrorInfo

initParseState :: Text -> State Text ErrorInfo
initParseState input =
  let posState = PosState input 0 (initialPos "NoFile") (mkPos 0) ""
   in State input 0 posState []

runParserStateM :: Text -> P a -> TParseMonad (PState, Either (ParseErrorBundle Text ErrorInfo) a)
runParserStateM input parser =
  let pstate = initParseState input :: PState
   in runParserT' parser pstate

runParser :: Text -> P a -> Maybe a
runParser input parser =
  let (parse_result, _state') = runState (runParserStateM input parser) initParseInfo
      (parse_state, parsed_val) = parse_result
      remainder = stateInput parse_state
   in if T.null remainder
        then either (error . show) Just parsed_val
        else traceShow remainder $ either (error . show) Just parsed_val

parseFile :: FilePath -> IO (Maybe (CompilationUnit Name))
parseFile file = do
  contents <- T.readFile file
  return $ Turu.Parser.runParser contents unit

-- Actual parsers

keywords :: [Text]
keywords = ["match", "in", "let", "fam", "Any", "rec"]

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

nameText :: P Text
nameText = do
  c <- lowerChar
  r <- many (alphaNumChar <|> char '_') :: P String
  space
  let n = pack (c : r)
  if n `P.elem` keywords
    then M.failure (Just $ Tokens $ NE.fromList $ T.unpack n) (S.singleton $ Label $ NE.fromList "identifier")
    else return n

name :: P Name
name = Name <$> nameText <*> getUnit

unitDef :: P UnitName
unitDef = do
  unit_str <- lex (tokens (==) "unit") *> lex nameText
  let unit_name = UnitName unit_str
  setUnit $ Just unit_name
  return unit_name

defs :: P [Either (Bind Name) (FamDef Name)]
defs = many def

def :: P (Either (Bind Name) (FamDef Name))
def = (Right <$> try famDef) <|> (Left <$> try bind)

unit :: P (CompilationUnit Name)
unit = do
  n <- unitDef
  (bs, fams) <- partitionEithers <$> defs
  void eof
  return $ Unit n bs fams

number :: P Int
number = lex $ L.signed mempty L.decimal

pleft :: P ()
pleft = void $ L.symbol space "("

pright :: P ()
pright = void $ L.symbol space ")"

aright :: P ()
aright = void $ sym "->"

famDef :: P (FamDef Name)
famDef = do
  key "fam"
  fam_name <- conNameP
  _ <- sym "="
  con_defs <- consList 0
  return $ FamDef fam_name con_defs

-- True | False | Either
consList :: Int -> P [ConDef Name]
consList tag = do
  con1 <- try $ consDef tag
  constrs <- try ((sym "|" *> (consList (tag + 1)) <|> pure []))
  return $ con1 : constrs

consDef :: Int -> P (ConDef Name)
consDef tag = do
  con_name <- conNameP
  con_args <- many conNameP
  return $ ConDef con_name tag con_args

bind :: P (Bind Name)
bind = rec_bind <|> (uncurry Bind) <$> bind1

rec_bind :: P (Bind Name)
rec_bind = do
  void $ key "rec" *> sym "{"
  binds <- manyTill bind1 (sym "}")
  return $ RecBinds binds

bind1 :: P (Name, Expr Name)
bind1 = do
  key "let"
  n <- name
  args <- many name
  _ <- sym "="
  e <- expr
  return $ (n, (mkLams args e))

type ExprP = P (Expr Name)

-- expression with or without parens
expr :: ExprP
expr = try (pleft *> expr1 <* pright) <|> try var <|> try con <|> try lit <|> try letp <|> try match

-- subexpression, only used where no parens can appear
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
  _ <- sym "="
  rhs <- expr
  key "in"
  body <- expr
  return $ Let (Bind n $ mkLams args rhs) body

conNameP :: P Name
conNameP = do
  c <- upperChar
  cs <- lex $ takeWhileP Nothing (isAlphaNum)
  let conName = c `cons` cs
  Name conName <$> (getUnit)

lam :: ExprP
lam = Lam <$> (sym "\\" *> name) <*> (aright *> expr)

var :: ExprP
var = Var <$> name

con :: ExprP
con = Var <$> conNameP

-- In order to make up work without parens we need to make it right(?)-recursive. I can't be bothered atm.
app :: ExprP
app = do
  f <- expr
  args <- some expr
  return $ App f args

match :: ExprP
match = Match <$> (key "match" *> name) <*> match_body

match_body :: P [Alt Name]
match_body = between (sym "[") (sym "]") alts <* space

alts :: P [Alt Name]
alts = sepBy1 (alt) (sym ",")

type AltP = P (Alt Name)

alt :: AltP
alt = litAlt <|> conAlt <|> wildAlt

litAlt, conAlt, wildAlt :: AltP
litAlt = LitAlt <$> (lit1 <* aright) <*> expr1
conAlt = ConAlt <$> conNameP <*> many name <*> (aright *> expr1)
wildAlt = WildAlt <$> (sym "_" *> aright *> expr1)

-- name :: P Name
-- name = do
--     c <- lowerChar :: P Char
--     r <- many alphaNumChar :: P Name
--     return $ c <> r
