module Turu.AST.Name where

import Turu.Prelude
import Turu.Pretty

import Data.Char as C
import Data.Hashable
import Data.String
import Data.Text as T
import GHC.Generics

-- | A unit of compilation is identified by it's **name**
newtype UnitName = UnitName {un_name :: Text} deriving (Eq, Ord, Hashable, Show, IsString)

instance Printable UnitName where
    ppr (UnitName n) = text $ show n

data Name = Name
    { n_name :: Text
    , n_unit :: Maybe UnitName
    -- ^ Nothing <-> it's a local id. Like arguments or local binders
    }
    deriving (Eq, Ord, Show)

instance Printable Name where
    ppr (Name{n_name, n_unit})
        | Just unit <- n_unit =
            ppr unit <> ":" <> ppr n_name
        | otherwise = ppr n_name

-- | Only local names
instance IsString Name where
    fromString s = Name (pack s) Nothing

instance Hashable Name where
    hash (Name{n_name, n_unit}) = hash (n_name, n_unit)
    hashWithSalt salt (Name{n_name, n_unit}) = hash (salt, n_name, n_unit)

mkLocalName :: Text -> Name
mkLocalName n = Name n Nothing

mkName :: UnitName -> Text -> Name
mkName unit n = Name n $ Just unit

isConName :: Name -> Bool
isConName name = isUpper $ T.head (n_name name)

------------ HasName utility class -----------
class HasName a where
    getName :: a -> Name
