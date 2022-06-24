module Main where

import Data.List
import Data.Ord
import GHC.IO
import System.Environment
import Text.Megaparsec.Debug (dbg)
import Text.Show.Pretty hiding (Name)

import qualified Data.Text as T
import Turu.AST
import Turu.AST.Name
import Turu.AST.Rename as R
import Turu.Eval.Reduce as Eval
import Turu.Parser as P
import Turu.Prelude

rnExpr1 :: Text -> Expr Var
rnExpr1 sexpr = runRn $ rnExpr $ fromMaybe (error "ParseError") $ runParser sexpr P.expr

rnUnit1 :: Text -> CompilationUnit Var
rnUnit1 s_unit = runRn $ rnUnit $ fromMaybe (error "ParseError") $ runParser s_unit P.unit

parseRenameFile :: FilePath -> CompilationUnit Var
parseRenameFile file = unsafePerformIO $ do
    parsed <- parseFile file
    putStrLn $ ppShow parsed
    let renamed = renameUnit <$> parsed
    pure $ fromMaybe (error $ "Failed to parse or rename:" <> file) renamed

parse :: a
parse = undefined

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No args"
        else do
            let file = head args
            let bind_name = if length args < 2 then "main" else args !! 2
            let main_unit = parseRenameFile file
            print $ evalBind (T.pack bind_name) main_unit
