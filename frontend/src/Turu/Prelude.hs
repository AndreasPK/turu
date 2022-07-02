module Turu.Prelude (
    module Prelude,
    module Data.Text,
    module Data.Foldable,
    module Data.Maybe,
    module Turu.Pretty,
    assert,
    module S,
    debugIsOn,
    concatMapM,
) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)
import GHC.Stack.Types as S (HasCallStack)
import Prelude

import Turu.Pretty (Printable (..))

assert :: Bool -> [Char] -> a -> a
assert b msg = if not b then error msg else id

debugIsOn :: Bool
debugIsOn = False

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
