module Turu.Pretty (
    module P,
    module Turu.Pretty,
) where

import Data.Text as T
import Text.PrettyPrint as P hiding ((<>))
import Text.Show.Pretty (ppShow)

class Printable a where
    ppr :: a -> Doc

instance Printable Text where
    ppr t = text $ T.unpack t

instance Printable Int where
    ppr x = text $ show x

instance (Printable a, Printable b) => Printable (a, b) where
    ppr (x, y) = parens (ppr x <> "," <> ppr y)

pprMaybe :: Printable a => Maybe a -> Doc
pprMaybe Nothing = mempty
pprMaybe (Just x) = ppr x

tshow :: Show a => a -> Text
tshow x = T.pack $ ppShow x
