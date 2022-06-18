module Turu.AST.Rename where

import Turu.Prelude

import Turu.AST

import Control.Monad.Trans.State.Strict as ST
import Data.Char as C
import Data.Text as T

type RnM a = State RnState a

type RnState = Int

nextUnique :: RnState -> (Int, RnState)
nextUnique s = (s, s + 1)

nextUniqueM :: RnM Int
nextUniqueM = do
    s <- get
    let (u, s') = nextUnique s
    put s'
    return u

mkNewVar :: Text -> RnM Var
mkNewVar name
    -- Constructor Var
    | C.isUpper first =
        undefined
    -- Other var
    | otherwise =
        undefined
  where
    first = T.head name

renameUnit :: CompilationUnit Text -> RnM (CompilationUnit Var)
renameUnit (Unit name binders) = Unit name <$> mapM rnBinder binders

rnBinder :: Bind Text -> RnM (Bind Var)
rnBinder = undefined
