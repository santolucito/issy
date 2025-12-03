module Test.Integration.CompileSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit

import Compiler (compile)
import Issy (parseLLIssyFormat)
import Test.TestUtils (readFixture, assertRight, isLeft)

tests :: TestTree
tests = testGroup "Compilation"
  [ testCase "compile sample.issy produces valid llissy" $ do
      input <- readFixture "docs/sample.issy"
      case compile input of
        Left err -> assertFailure $ "Compilation failed: " ++ err
        Right output -> do
          _ <- assertRight $ parseLLIssyFormat output
          return ()

  , testCase "compile preserves basic structure" $ do
      input <- readFixture "docs/sample.issy"
      let Right llissy = compile input
      -- Check that output contains expected elements
      assertBool "Should contain variable declarations" $
        "(input Int i)" `isInfixOf` llissy || "(state Int" `isInfixOf` llissy
      assertBool "Should contain Safety objective" $
        "Safety" `isInfixOf` llissy

  , testCase "compile error - undefined variable" $ do
      let input = unlines
            [ "input int x"
            , "formula {"
            , "    assert F [undefined_var > 0]"
            , "}"
            ]
      assertBool "Should fail for undefined variable" $ isLeft $ compile input

  , testCase "compiled duplicate variable rejected by parser" $ do
      -- The compiler passes through duplicates, but the LLIssy parser rejects them
      let input = unlines
            [ "input int x"
            , "input int x"  -- duplicate
            ]
      case compile input of
        Left _ -> return ()  -- Compiler might reject
        Right llissy ->
          -- Even if compiler passes it through, parsing should fail
          assertBool "LLIssy parser should reject duplicate" $ isLeft $ parseLLIssyFormat llissy

  , testCase "compile minimal valid spec" $ do
      let input = unlines
            [ "input int x"
            , "formula {"
            , "    assert F [x > 0]"
            , "}"
            ]
      case compile input of
        Left err -> assertFailure $ "Should compile minimal spec: " ++ err
        Right output -> do
          _ <- assertRight $ parseLLIssyFormat output
          return ()

  , testCase "compile with macros" $ do
      let input = unlines
            [ "input int i"
            , "def bound = [10]"
            , "formula {"
            , "    assert F [i > bound]"
            , "}"
            ]
      case compile input of
        Left err -> assertFailure $ "Should compile with macros: " ++ err
        Right _ -> return ()

  , testCase "compile game spec" $ do
      let input = unlines
            [ "input int i"
            , "state int x"
            , "game Safety from l0 {"
            , "    loc l0 1"
            , "    from l0 to l0 with [x' = x + i]"
            , "}"
            ]
      case compile input of
        Left err -> assertFailure $ "Should compile game spec: " ++ err
        Right output -> do
          _ <- assertRight $ parseLLIssyFormat output
          assertBool "Should contain game" $ "Safety" `isInfixOf` output
  ]
