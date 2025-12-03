module Test.Integration.ParsePrintSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Issy (parseLLIssyFormat, printLLIssyFormat)
import Test.TestUtils (readFixture, assertRight)

tests :: TestTree
tests = testGroup "Parse/Print Roundtrip"
  [ testCase "llissy: parse -> print -> parse yields valid output" $ do
      input <- readFixture "docs/sample.llissy"
      spec1 <- assertRight $ parseLLIssyFormat input
      let printed = printLLIssyFormat spec1
      _ <- assertRight $ parseLLIssyFormat printed
      return ()

  , testCase "parse minimal llissy spec" $ do
      let input = "(()()())"
      spec <- assertRight $ parseLLIssyFormat input
      let printed = printLLIssyFormat spec
      _ <- assertRight $ parseLLIssyFormat printed
      return ()

  , testCase "parse llissy with variables" $ do
      let input = unlines
            [ "("
            , "  ((input Int x)(state Bool y))"
            , "  ()"
            , "  ()"
            , ")"
            ]
      spec <- assertRight $ parseLLIssyFormat input
      let printed = printLLIssyFormat spec
      _ <- assertRight $ parseLLIssyFormat printed
      return ()

  , testCase "parse llissy with formula" $ do
      let input = unlines
            [ "("
            , "  ((input Int i))"
            , "  ((()((F (ap (> i 0))))))"
            , "  ()"
            , ")"
            ]
      spec <- assertRight $ parseLLIssyFormat input
      let printed = printLLIssyFormat spec
      _ <- assertRight $ parseLLIssyFormat printed
      return ()

  , testCase "parse llissy with game" $ do
      let input = unlines
            [ "("
            , "  ((state Int x))"
            , "  ()"
            , "  (("
            , "    ((l0 0 true))"
            , "    ((l0 l0 true))"
            , "    (l0 Safety)"
            , "  ))"
            , ")"
            ]
      spec <- assertRight $ parseLLIssyFormat input
      let printed = printLLIssyFormat spec
      _ <- assertRight $ parseLLIssyFormat printed
      return ()

  , testCase "roundtrip produces valid output" $ do
      input <- readFixture "docs/sample.llissy"
      spec1 <- assertRight $ parseLLIssyFormat input
      let printed1 = printLLIssyFormat spec1
      spec2 <- assertRight $ parseLLIssyFormat printed1
      let printed2 = printLLIssyFormat spec2
      -- Third parse should succeed (stable)
      _ <- assertRight $ parseLLIssyFormat printed2
      return ()
  ]
