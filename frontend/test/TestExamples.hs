module TestExamples where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Ord
import GHC.IO
import System.Directory

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

import System.FilePath
import TestFile

-- Get source files which have expected results
getTestFiles :: FilePath -> IO [FilePath]
getTestFiles folder = do
    files <- listDirectory folder
    let src_files = filter (\f -> takeExtension f == ".turu") files

    let tests_todo = filter (\f -> addExtension f ".result" `elem` files) src_files

    return $ map (folder </>) tests_todo

fileTests :: IO TestTree
fileTests = do
    files <- getTestFiles "examples"
    return $ testGroup "fileTests" $ map mkFileTest files

mkFileTest :: FilePath -> TestTree
mkFileTest file =
    testCase ("fileTest:" <> file) $ checkOutput file