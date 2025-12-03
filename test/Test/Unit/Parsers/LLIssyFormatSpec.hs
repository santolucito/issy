module Test.Unit.Parsers.LLIssyFormatSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Issy (parseLLIssyFormat)
import qualified Issy.Specification as Spec
import qualified Issy.Base.Variables as Vars
import Test.TestUtils (isLeft, assertRight)

tests :: TestTree
tests = testGroup "LLIssyFormat Parser"
  [ testGroup "Basic structure"
    [ testCase "parse minimal valid spec" $ do
        let input = "(()()())"
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "no variables" 0 (length $ Vars.allSymbols $ Spec.variables spec)
        assertEqual "no formulas" 0 (length $ Spec.formulas spec)
        assertEqual "no games" 0 (length $ Spec.games spec)

    , testCase "parse empty with whitespace" $ do
        let input = "(\n  ()\n  ()\n  ()\n)"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse with comments" $ do
        let input = "(\n  ; comment\n  ()\n  ()\n  ()\n)"
        _ <- assertRight $ parseLLIssyFormat input
        return ()
    ]

  , testGroup "Variable declarations"
    [ testCase "parse input Int variable" $ do
        let input = "(((input Int x))()())"
        spec <- assertRight $ parseLLIssyFormat input
        assertBool "has input x" $ "x" `elem` Vars.inputL (Spec.variables spec)

    , testCase "parse state Bool variable" $ do
        let input = "(((state Bool y))()())"
        spec <- assertRight $ parseLLIssyFormat input
        assertBool "has state y" $ "y" `elem` Vars.stateVarL (Spec.variables spec)

    , testCase "parse multiple variables" $ do
        let input = "(((input Int i)(state Int x)(state Bool b))()())"
        spec <- assertRight $ parseLLIssyFormat input
        let vars = Spec.variables spec
        -- 3 declared vars, but state vars have primed versions (x~, b~)
        assertBool "has input i" $ "i" `elem` Vars.inputL vars
        assertBool "has state x" $ "x" `elem` Vars.stateVarL vars
        assertBool "has state b" $ "b" `elem` Vars.stateVarL vars
        assertEqual "input count" 1 (length $ Vars.inputL vars)
        assertEqual "state count" 2 (length $ Vars.stateVarL vars)

    , testCase "parse Real variable" $ do
        let input = "(((state Real r))()())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()
    ]

  , testGroup "Formulas"
    [ testCase "parse simple formula" $ do
        let input = "(((input Int i))((()((F (ap (> i 0))))))())"
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "one formula" 1 (length $ Spec.formulas spec)

    , testCase "parse formula with assumptions" $ do
        let input = "(((input Int i))((((F (ap true)))((F (ap (> i 0))))))())"
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "one formula" 1 (length $ Spec.formulas spec)

    , testCase "parse multiple formulas" $ do
        let input = "(((input Int i))((()((F (ap (> i 0)))))(()((G (ap (>= i 0))))))())"
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "two formulas" 2 (length $ Spec.formulas spec)
    ]

  , testGroup "Games"
    [ testCase "parse simple game" $ do
        let input = "(((state Int x))()((((l0 0 true))((l0 l0 true))(l0 Safety))))"
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "one game" 1 (length $ Spec.games spec)

    , testCase "parse game with multiple locations" $ do
        let input = unlines
              [ "("
              , "  ((state Int x))"
              , "  ()"
              , "  (("
              , "    ((l0 0 true)(l1 1 true))"
              , "    ((l0 l0 true)(l0 l1 true)(l1 l1 true))"
              , "    (l0 Safety)"
              , "  ))"
              , ")"
              ]
        spec <- assertRight $ parseLLIssyFormat input
        assertEqual "one game" 1 (length $ Spec.games spec)

    , testCase "parse game with Reachability objective" $ do
        let input = "(((state Int x))()((((l0 1 true))((l0 l0 true))(l0 Reachability))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse game with Buechi objective" $ do
        let input = "(((state Int x))()((((l0 1 true))((l0 l0 true))(l0 Buechi))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse game with CoBuechi objective" $ do
        let input = "(((state Int x))()((((l0 0 true))((l0 l0 true))(l0 CoBuechi))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()
    ]

  , testGroup "Terms"
    [ testCase "parse true/false" $ do
        let input = "(((state Bool b))()((((l0 0 true))((l0 l0 false))(l0 Safety))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse integer constants" $ do
        let input = "(((state Int x))()((((l0 0 (> x 42)))((l0 l0 true))(l0 Safety))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse arithmetic expressions" $ do
        let input = "(((input Int i)(state Int x))()((((l0 0 true))((l0 l0 (= x~ (+ x i))))(l0 Safety))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse comparison operators" $ do
        let input = "(((state Int x))()((((l0 0 (and (>= x 0) (<= x 10))))((l0 l0 true))(l0 Safety))))"
        _ <- assertRight $ parseLLIssyFormat input
        return ()
    ]

  , testGroup "Error cases"
    [ testCase "reject invalid structure" $ do
        assertBool "should fail" $ isLeft $ parseLLIssyFormat "(())"
        assertBool "should fail" $ isLeft $ parseLLIssyFormat "(()()()(extra))"

    , testCase "reject duplicate variable" $ do
        let input = "(((input Int x)(input Int x))()())"
        assertBool "should reject duplicate" $ isLeft $ parseLLIssyFormat input

    , testCase "reject unknown sort" $ do
        let input = "(((input Unknown x))()())"
        assertBool "should reject unknown sort" $ isLeft $ parseLLIssyFormat input

    , testCase "reject undeclared variable in formula" $ do
        let input = "(()((()(( F (ap (> undefined 0))))))())"
        assertBool "should reject undeclared" $ isLeft $ parseLLIssyFormat input

    , testCase "reject unknown winning condition" $ do
        let input = "(((state Int x))()((((l0 0 true))((l0 l0 true))(l0 Unknown))))"
        assertBool "should reject unknown wc" $ isLeft $ parseLLIssyFormat input
    ]

  , testGroup "Temporal operators"
    [ testCase "parse F (eventually)" $ do
        let input = "(((input Int i))((()((F (ap (> i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse G (globally)" $ do
        let input = "(((input Int i))((()((G (ap (> i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse X (next)" $ do
        let input = "(((input Int i))((()((X (ap (> i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse U (until)" $ do
        let input = "(((input Int i))((()((U (ap (> i 0)) (ap (= i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse W (weak until)" $ do
        let input = "(((input Int i))((()((W (ap (> i 0)) (ap (= i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()

    , testCase "parse R (release)" $ do
        let input = "(((input Int i))((()((R (ap (> i 0)) (ap (= i 0))))))())"
        _ <- assertRight $ parseLLIssyFormat input
        return ()
    ]
  ]
