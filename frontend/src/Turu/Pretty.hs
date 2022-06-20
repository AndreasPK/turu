module Turu.Pretty (
    module P,
    module Turu.Pretty,
) where

import Data.Text
import Text.PrettyPrint as P hiding ((<>))

class Printable a where
    ppr :: a -> Doc

instance Printable Text where
    ppr t = text $ show t

instance Printable Int where
    ppr x = text $ show x

instance (Printable a, Printable b) => Printable (a, b) where
    ppr (x, y) = parens (ppr x <> "," <> ppr y)

pprMaybe :: Printable a => Maybe a -> Doc
pprMaybe Nothing = mempty
pprMaybe (Just x) = ppr x