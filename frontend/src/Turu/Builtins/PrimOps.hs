{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}

module Turu.Builtins.PrimOps where

import Turu.Prelude

import Data.Char (toLower, toUpper)
import qualified Data.Data as D
import Data.HashMap.Strict as M
import qualified Data.Text as T
import Turu.AST.Name
import Turu.Pretty

-----------

data PrimOp
    = AddInt
    | Print
    | ReadInt -- Takes a dummy argument
    | ReadStr -- Takes a dummy argument
    | StrLen
    | HeadStr -- op "abc" = ord a
    | TailStr -- op "abc" = "bc"
    | ConsStr -- op a "bc" = (chr a):"bc"
    | SnocStr -- op "ab" c = "ab" <> "c"
    deriving (D.Data, Eq, Show, Ord)

instance Printable PrimOp where
    ppr :: PrimOp -> Doc
    ppr = text . show

-- Works for nullary primops
primopNames :: [Name]
primopNames =
    let cons = case D.dataTypeRep (D.dataTypeOf (undefined :: PrimOp)) of
            D.AlgRep cons' -> cons'
            _ -> error "impossible"
        names = fmap D.showConstr cons
        mkTuruPrimName (c : cs) = mkBuiltinName $ T.pack $ (toLower c) : cs
        mkTuruPrimName _ = error "impossible"
     in fmap mkTuruPrimName names

namePrimOp :: Name -> Maybe PrimOp
namePrimOp name =
    let name_str = (T.unpack $ n_name name)
        con = D.readConstr (D.dataTypeOf (undefined :: PrimOp)) (upperHead name_str) :: Maybe D.Constr
     in D.fromConstr <$> con

upperHead :: String -> String
upperHead (c : cs) = toUpper c : cs
upperHead s = s