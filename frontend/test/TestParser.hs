module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Turu.AST
import Turu.AST.Name
import Turu.AST.Rename as R
import Turu.Eval.Reduce as Eval
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

rnExpr1 :: Text -> Expr Var
rnExpr1 sexpr = runRn $ rnExpr $ fromMaybe (error "ParseError") $ runParser sexpr P.expr

rnUnit1 :: Text -> CompilationUnit Var
rnUnit1 s_unit = runRn $ rnUnit $ fromMaybe (error "ParseError") $ runParser s_unit P.unit

unitTests :: TestTree
unitTests =
    testGroup
        "tests"
        [ testGroup
            "Unit tests"
            [ testCase "application" $
                let result = App "a" ["b"] :: Expr Name
                 in runParser "(a b)" P.expr @?= Just result
            , testCase "match" $
                let result = Match "s" [ConAlt "Con" ["x", "y"] "x"] :: Expr Name
                 in runParser "match s [Con x y -> x]" P.expr @?= Just result
            , testCase "bind" $
                let result = Bind "f" (Lam "x" "x") :: Bind Name
                 in runParser "f x = x" P.bind @?= Just result
            , testCase "let" $
                let result = Let (Bind "f" (Lam "x" "x")) (App "f" ["y"]) :: Expr Name
                 in runParser "let f x = x in (f y)" P.expr @?= Just result
            , testCase "lam" $
                let result = Lam "x" "x" :: Expr Name
                 in runParser "(\\x -> x)" P.expr @?= Just result
            , testCase "let2" $
                let result = Let (Bind "f" (Lam "y" $ Lam "x" "x")) (App "f" ["y"]) :: Expr Name
                 in runParser "let f y x = x in (f y)" P.expr @?= Just result
            , testCase "unit1" $
                let result = Unit "myUnit" [] []
                 in runParser "unit myUnit \n" P.unit @?= Just result
            , testCase "unit2" $
                let result = Unit "myUnit" [Bind (mkName "myUnit" "f") $ Lam (mkName "myUnit" "x") (Var $ mkName "myUnit" "x")] []
                 in runParser "unit myUnit \nf x = x" P.unit @?= Just result
            , testCase "conDef" $
                let result = FamDef "Bool" [ConDef "True" 0 [], ConDef "False" 1 []]
                 in runParser "fam Bool = True | False" P.famDef @?= Just result
            , testCase "conDef - maybe" $
                let result = FamDef "Maybe" [ConDef "Just" 0 ["Any"], ConDef "Nothing" 1 []]
                 in runParser "fam Maybe = Just Any | Nothing" P.famDef @?= Just result
            ]
        , testGroup
            "Eval tests"
            [ testCase "lam1" $
                let result = Obj (LitInt 1) :: Closure
                 in evalExpr (rnExpr1 "((\\x -> x) 1)") @?= result
            , testCase "lam2" $
                let result = Obj (LitInt 1) :: Closure
                    unit =
                        ( rnUnit1
                            ( "unit myUnit \n" <> "fam AB = A | B\n" <> "f = let a = A in match a [A ->1, B->2]"
                            )
                        )
                    expr = Var $ MkVar 0 (mkName "myUnit" "f") simpValInfo
                 in (evalWithUnit expr unit) @?= result
            ]
        ]
