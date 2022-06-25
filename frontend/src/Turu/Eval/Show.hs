{-# LANGUAGE MultiParamTypeClasses #-}

module Turu.Eval.Show where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as TS
import Data.Bits (Bits (xor))
import Data.Char
import Data.Coerce
import Data.Map.Lazy as M hiding (update)
import Data.Set as S
import Data.String
import qualified Data.Text as T
import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)

import Data.List (intercalate)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Utils
import Turu.Eval.Reduce
import Turu.Eval.Types
import Turu.Prelude as P
import Turu.Pretty

showClosure :: Bool -> Closure -> Doc
showClosure show_env closure =
    case closure of
        Ind name -> "Ind[" <> ppr name <> "]"
        Obj l -> ppr l
        SatCon c args -> ppr (getName c) <> args'
          where
            args'
                | P.null args = mempty
                | otherwise = parens (vcat $ fmap (showClosure show_env) args)
        Fun code -> ppr code
        FunClosure code dat ->
            ppr code
                <> if show_env
                    then text $ ppShow dat
                    else mempty
