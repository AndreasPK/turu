module TestFile where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec.Debug (dbg)
import Text.Show.Pretty hiding (Name)

import Control.Monad
import Turu.AST
import Turu.AST.Name
import Turu.AST.Var
import Turu.AST.Rename as R
import Turu.Tc.Type
import Turu.Eval.Reduce as Eval
import Turu.Eval.Show (showClosure)
import Turu.Parser as P
import Turu.Prelude
import Turu.Pretty

rnExpr1 :: Text -> Expr Var
rnExpr1 sexpr = runRn $ rnExpr $ fromMaybe (error "ParseError") $ runParser sexpr P.expr

rnUnit1 :: Text -> CompilationUnit Var
rnUnit1 s_unit = runRn $ rnUnit $ fromMaybe (error "ParseError") $ runParser s_unit P.unit

parseRenameFile :: FilePath -> CompilationUnit Var
parseRenameFile file = unsafePerformIO $ do
    parsed <- parseFile file
    -- when debugIsOn $ putStrLn $ ppShow parsed
    let renamed = renameUnit <$> parsed
    pure $ fromMaybe (error $ "Failed to parse or rename:" <> file) renamed

-- Check if executing file foo.turu matches the output stored in foo.turu.result
checkOutput :: FilePath -> Assertion
checkOutput src = do
    let -- src = fp </> ".turu"
        out = src <.> ".result"
        unit = parseRenameFile src
        result = either id (\r -> (T.pack . render . showClosure False) r <> "\n") $ evalMain unit
    -- print out
    output <- T.readFile out
    assertEqual "checkOutput" output result
