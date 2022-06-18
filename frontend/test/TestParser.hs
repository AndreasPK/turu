module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Turu.AST
import Turu.Parser as P

import Data.List
import Data.Ord
import Turu.Prelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

unitTests =
    testGroup
        "Unit tests"
        [ testCase "application" $
            let result = App "a" ["b"] :: Expr Text
             in runParser "(a b)" P.expr @?= Just result
        , testCase "match" $
            let result = Match "s" [ConAlt "Con" ["x", "y"] "x"] :: Expr Text
             in runParser "match s [Con x y -> x]" P.expr @?= Just result
        , testCase "bind" $
            let result = Bind "f" (Lam "x" "x") :: Bind Text
             in runParser "f x = x" P.bind @?= Just result
        , testCase "let" $
            let result = Let (Bind "f" (Lam "x" "x")) (App "f" ["y"]) :: Expr Text
             in runParser "let f x = x in (f y)" P.expr @?= Just result
        , testCase "let2" $
            let result = Let (Bind "f" (Lam "y" $ Lam "x" "x")) (App "f" ["y"]) :: Expr Text
             in runParser "let f y x = x in (f y)" P.expr @?= Just result
        ]