module Turu.Prelude (
    module Prelude,
    module Data.Text,
    module Data.Foldable,
    module Data.Maybe,
    module Turu.Pretty,
    assert,
) where

import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Prelude

import Turu.Pretty (Printable (..))

assert :: Bool -> [Char] -> a -> a
assert b msg = if not b then error msg else id