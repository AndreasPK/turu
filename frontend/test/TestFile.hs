module TestFile where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Ord
import GHC.IO
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec.Debug (dbg)
import Text.Show.Pretty hiding (Name)
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

checkOutput :: FilePath -> IO Bool
checkOutput fp = do
    let src = fp </> ".turu"
        out = fp </> ".out"
        unit = parseRenameFile src
        result = evalMain unit
    output <- readFile out
    if output == ppShow result
        then pure True
        else do
            putStrLn $ "Missmatched output for " <> fp
            putStrLn "Expected:"
            putStrLn $ ppShow output
            putStrLn "Actual:"
            putStrLn $ ppShow result
            return False
