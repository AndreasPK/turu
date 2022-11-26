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
import Misc

import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.AST.Var
import Turu.Tc.Type
import Turu.Prelude as P hiding (lex, takeWhile)

import qualified Data.List.NonEmpty as NE
{-

--------------------------
------ imports
--------------------------

import <unitName>

------------------------

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

tyAnnot = ['<' tyP '>'] -- optional

tyP  = numTy    -- x<1>
    | funTy     -- x<1 -> 2 -> 0 -> 1>
    | varTy     -- x<n>

funTy = tys

tys = tyP -> tys
    | empty

numTy = number
varTy = name

tyExpr = expr tyAnnot'<'tyP'>'

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
    { pi_unit :: (Maybe UnitName)
    , pi_binds :: ~[Bind Text]
    , pi_uniq :: Int
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
    MTLS.put $ s{pi_unit = unit1}

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

parseFile :: FilePath -> IO (Maybe (CompilationUnit NameT))
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

rarrow :: P Text
rarrow = tokens (==) "->"

-- Consume white space (including newlines)
space :: P ()
space = L.space space1 lineComment blockComment

-- | Consume whitespace after thing
lex :: P a -> P a
lex = L.lexeme space

sym :: Text -> P Text
sym = L.symbol space

-- Parse a keyword
key :: Text -> P ()
key word = (string word *> notFollowedBy alphaNumChar) <* space

-- | parse identifier without consuming space
identifier :: P Text
identifier = do
    c <- lowerChar
    r <- many (alphaNumChar <|> char '_' <|> char '\'') :: P String
    let n = pack (c : r)
    if n `P.elem` keywords
        then M.failure (Just $ Tokens $ NE.fromList $ T.unpack n) (S.singleton $ Label $ NE.fromList "identifier")
        else return n

-- | parse identifier without consuming space
conIdentifier :: P Text
conIdentifier = do
    c <- upperChar
    cs <- lex $ takeWhileP Nothing (isAlphaNum)
    let conName = c `cons` cs
    return conName

modulePrefix :: P UnitName
modulePrefix = UnitName <$> identifier <* char '.'

name :: P Text -> P Name
name name_kind =
    try
        ( do
            u <- modulePrefix
            n <- name_kind <* space
            pure (Name n (Just u))
        )
        <|> Name <$> (name_kind <* space) <*> getUnit

-- | Variable use [with type]
valName :: P NameT
valName = pnamety (name identifier) tyAnnot

conNameP :: P NameT
conNameP = do
    pnamety (name conIdentifier) tyAnnot

-- Dropes any type annotation
conUseP :: P DataCon
conUseP = ParsedCon <$> (getName <$> conNameP)

unitDef :: P UnitName
unitDef = do
    unit_str <- lex (tokens (==) "unit") *> lex identifier
    let unit_name = UnitName unit_str
    setUnit $ Just unit_name
    return unit_name

defs :: P [Either (Bind NameT) (FamDef NameT)]
defs = many def

def :: P (Either (Bind NameT) (FamDef NameT))
def = (Right <$> try famDef) <|> (Left <$> try bind)

--- Some utility functions to mix it up
---------------------------------------
endOfFile :: P ()
endOfFile = void eof

parens :: P a -> P a
parens = between pleft pright

pnamety :: P Name -> P (Maybe Type) -> P NameT
pnamety l r = pure MkNameT <*> l <*> r
-- More ast parsing
-------------------

unit :: P (CompilationUnit NameT)
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

famDef :: P (FamDef NameT)
famDef = do
    key "fam"
    fam_name <- conNameP
    _ <- sym "="
    con_defs <- consList 0
    return $ FamDef fam_name con_defs

-- True | False | Either
consList :: Int -> P [ConDef NameT]
consList tag = do
    con1 <- try $ consDef tag
    constrs <- try ((sym "|" *> (consList (tag + 1)) <|> pure []))
    return $ con1 : constrs

consDef :: Int -> P (ConDef NameT)
consDef tag = do
    con_name <- conNameP
    con_args <- many conNameP
    return $ ConDef con_name tag con_args

bind :: P (Bind NameT)
bind = rec_bind <|> (uncurry Bind) <$> bind1

rec_bind :: P (Bind NameT)
rec_bind = do
    void $ key "rec" *> sym "{"
    binds <- manyTill bind1 (sym "}")
    return $ RecBinds binds

bind1 :: P (NameT, Expr NameT)
bind1 = do
    key "let"
    n <- valName
    args <- many (valName)
    _ <- sym "="
    -- Not sure if using expr1 here is save
    e <- expr1
    return $ (n, (mkLams args e))

type ExprP = P (Expr NameT)

-- | x<T> - adds type to the var
varTypeSuffix :: Var -> P Var
varTypeSuffix v = do
    annot <- try tyAnnot
    return $ v { v_ty = annot}

tyAnnot :: P (Maybe Type)
tyAnnot = lex (char '<') *> pure Just <*> tyP <* lex(char '>') <|> pure Nothing

-- The type inside the <> brackets
tyP :: P (Type)
tyP = do
    lex (varType <|> try funType <|> (numType))

tys :: P [Type]
tys = do
    t1 <- (numType <|> varType <|> parens tyP) <* space
    ts <- try $ (lex rarrow *> space *> tys) <|> pure []
    pure (t1:ts)

numType :: P Type
numType = do
    n <- lex number
    pure $! assert (n >= 0) "numType >= 0" True
    return $ mkArityOnlyTy n

funType :: P Type
funType = do
    arr_tys <- tys
    (arg_tys,res_ty) <-
        case snocView arr_tys of
            Just view -> return view
            Nothing -> fail "bla"

    when (P.null arg_tys) $ fail "funType - not enough args"
    let arity = P.length arg_tys
    return $ mkFunTy arity (Just arg_tys) res_ty

varType :: P Type
varType = do
    v_name <- name identifier
    return $ TyVar v_name



-- expression with or without parens
expr :: ExprP
expr = try var <|> try con <|> try lit <|> try letp <|> try match <|> try (between pleft pright expr1)

-- subexpression, without required parens
expr1 :: ExprP
expr1 = try app <|> try lam <|> expr

lit :: ExprP
lit = Lit <$> lit1

lit1 :: P Literal
lit1 = (slit <|> nlit <|> clit) <* space

slit, nlit, clit :: P Literal
slit = LitString <$> between (char '"') (char '"') (takeWhileP (Just "String") (/= '"'))
nlit = LitInt <$> number
clit = LitChar <$> between (char '\'') (char '\'') anySingle

-- unitlit = sym "()" *> pure LitUnit

letp :: ExprP
letp = do
    key "let"
    n <- valName
    args <- many (valName)
    _ <- sym "="
    rhs <- expr1
    key "in"
    body <- expr1
    return $ Let (Bind n $ mkLams args rhs) body

lam :: ExprP
lam = do
    _ <- sym "\\"
    b <- valName
    body <- (aright *> expr)
    return $ Lam b body

-- | var expr
var :: ExprP
var = Var <$> valName

-- | con expr
con :: ExprP
con = Var <$> conNameP

-- In order to make up work without parens we need to make it right(?)-recursive. I can't be bothered atm.
app :: ExprP
app = do
    f <- expr
    args <- some expr
    return $ App f args

match :: ExprP
match = Match <$> (key "match" *> valName) <*> match_body

match_body :: P [Alt NameT]
match_body = between (sym "[") (sym "]") alts <* space

alts :: P [Alt NameT]
alts = sepBy1 (alt) (sym ",")

type AltP = P (Alt NameT)

alt :: AltP
alt = litAlt <|> conAlt <|> wildAlt

litAlt, conAlt, wildAlt :: AltP
litAlt = LitAlt <$> (lit1 <* aright) <*> expr1
conAlt = ConAlt <$> conUseP <*> many valName <*> (aright *> expr1)
wildAlt = WildAlt <$> (sym "_" *> aright *> expr1)

-- name :: P NameT
-- name = do
--     c <- lowerChar :: P Char
--     r <- many alphaNumChar :: P NameT
--     return $ c <> r
