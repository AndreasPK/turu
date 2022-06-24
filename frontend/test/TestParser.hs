module Main where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Ord
import GHC.IO
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec.Debug (dbg)
import Text.Show.Pretty hiding (Name)
import Turu.AST
import Turu.AST.Name
import Turu.AST.Rename as R
import Turu.Eval.Reduce as Eval
import Turu.Parser as P
import Turu.Prelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

rnExpr1 :: Text -> Expr Var
rnExpr1 sexpr = runRn $ rnExpr $ fromMaybe (error "ParseError") $ runParser sexpr P.expr

rnUnit1 :: Text -> CompilationUnit Var
rnUnit1 s_unit = runRn $ rnUnit $ fromMaybe (error "ParseError") $ runParser s_unit P.unit

parseRenameFile :: FilePath -> CompilationUnit Var
parseRenameFile file = unsafePerformIO $ do
    parsed <- parseFile file
    putStrLn $ ppShow parsed
    let renamed = renameUnit <$> parsed
    pure $ fromMaybe (error $ "Failed to parse or rename:" <> file) renamed

unitTests :: TestTree
unitTests =
    testGroup
        "tests"
        [ testGroup
            "Parse tests"
            [ testCase "application" $
                let result = App "a" ["b"] :: Expr Name
                 in runParser "(a b)" P.expr @?= Just result
            , testCase "match" $
                let result = Match "s" [ConAlt "Con" ["x", "y"] "x"] :: Expr Name
                 in runParser "match s [Con x y -> x]" P.match @?= Just result
            , testCase "match-expr" $
                let result = Match "s" [ConAlt "Con" ["x", "y"] "x"] :: Expr Name
                 in runParser "match s [Con x y -> x]" (dbg "" P.expr) @?= Just result
            , testCase "bind" $
                let result = Bind "f" (Lam "x" "x") :: Bind Name
                 in runParser "let f x = x" P.bind @?= Just result
            , testCase "rec-bind" $
                let result = RecBinds [("f", (Lam "x" "x"))] :: Bind Name
                 in runParser "rec { let f x = x }" (dbg "" P.bind) @?= Just result
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
                 in runParser "unit myUnit \nlet f x = x" P.unit @?= Just result
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
                            ( "unit myUnit \n" <> "fam AB = A | B\n" <> "let f = let a = A in match a [A ->1, B->2]"
                            )
                        )
                    expr = Var $ MkVar 0 (mkName "myUnit" "f") simpValInfo
                 in (evalWithUnit expr unit) @?= result
            , testCase "lam3" $
                let result = Obj (LitInt 1) :: Closure
                    unit =
                        ( rnUnit1
                            ( "unit myUnit \n" <> "fam AB = A | B\n" <> "rec { let f = let a = A in match a [A ->1, B->2] }"
                            )
                        )
                    expr = Var $ MkVar 0 (mkName "myUnit" "f") simpValInfo
                 in (evalWithUnit expr unit) @?= result
                -- testCase "rec-lam" $
                --   let parsed_unit = unsafePerformIO $ do
                --         m_unit <- parseFile "test/twice.turu"
                --         putStrLn $ ppShow m_unit
                --         return $ fromMaybe (error "Parse failed in testCase 'rec-lam'") m_unit
                --       renamed_unit = renameUnit parsed_unit
                --       result = Right $ Obj $ LitInt 0
                --    in (evalMain renamed_unit) @?= result,
                -- testCase "rec-lam" $
                --   let unit = parseRenameFile "test/list.turu"
                --       result = Right $ Obj $ LitInt 0
                --    in (evalMain unit) @?= result
            ]
        ]
