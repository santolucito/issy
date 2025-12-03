module Test.Integration.SolveSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Issy
import Issy.Config (Solver(..))
import Test.TestUtils (readFixture, requireZ3, requireCVC5, assertRight)

tests :: TestTree
tests = testGroup "Solving"
  [ testCase "solve sample.llissy specification" $ do
      requireZ3
      input <- readFixture "docs/sample.llissy"
      spec <- assertRight $ parseLLIssyFormat input
      result <- checkSpecification defaultConfig spec
      case result of
        Left err -> assertFailure $ "Specification check failed: " ++ err
        Right () -> return ()

  , testCase "solve minimal game" $ do
      requireZ3
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
      result <- checkSpecification defaultConfig spec
      case result of
        Left err -> assertFailure $ "Specification check failed: " ++ err
        Right () -> return ()

  -- Note: This test requires Z3 with solver-subsumption tactic support
  -- Skipped as it may fail on older Z3 versions
  --, testCase "full pipeline: parse -> translate -> solve" $ do
  --    requireZ3
  --    let input = unlines
  --          [ "("
  --          , "  ((input Int i)(state Int x))"
  --          , "  ()"
  --          , "  (("
  --          , "    ((l0 0 true))"
  --          , "    ((l0 l0 (= x~ x)))"
  --          , "    (l0 Safety)"
  --          , "  ))"
  --          , ")"
  --          ]
  --    spec <- assertRight $ parseLLIssyFormat input
  --    sgGame <- specToSG defaultConfig spec
  --    let game = fromSG sgGame
  --    (solveResult, _stats, _strat) <- solve defaultConfig emptyStats game
  --    -- A trivial game where x' = x should be realizable
  --    assertBool "Trivial safety game should be realizable" solveResult

  , testCase "solve with different objectives" $ do
      requireZ3
      -- Test Safety objective
      let safetyInput = unlines
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
      specSafety <- assertRight $ parseLLIssyFormat safetyInput
      safetyResult <- checkSpecification defaultConfig specSafety
      case safetyResult of
        Left err -> assertFailure $ "Safety spec check failed: " ++ err
        Right () -> return ()

      -- Test Reachability objective
      let reachInput = unlines
            [ "("
            , "  ((state Int x))"
            , "  ()"
            , "  (("
            , "    ((l0 1 true))"
            , "    ((l0 l0 true))"
            , "    (l0 Reachability)"
            , "  ))"
            , ")"
            ]
      specReach <- assertRight $ parseLLIssyFormat reachInput
      reachResult <- checkSpecification defaultConfig specReach
      case reachResult of
        Left err -> assertFailure $ "Reachability spec check failed: " ++ err
        Right () -> return ()

  -- CVC5 tests
  , testCase "solve sample.llissy with CVC5" $ do
      requireCVC5
      input <- readFixture "docs/sample.llissy"
      spec <- assertRight $ parseLLIssyFormat input
      let cvc5Config = defaultConfig { solver = CVC5 }
      result <- checkSpecification cvc5Config spec
      case result of
        Left err -> assertFailure $ "Specification check failed with CVC5: " ++ err
        Right () -> return ()

  , testCase "solve minimal game with CVC5" $ do
      requireCVC5
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
      let cvc5Config = defaultConfig { solver = CVC5 }
      result <- checkSpecification cvc5Config spec
      case result of
        Left err -> assertFailure $ "Specification check failed with CVC5: " ++ err
        Right () -> return ()

  , testCase "CVC5 solves safety and reachability objectives" $ do
      requireCVC5
      let cvc5Config = defaultConfig { solver = CVC5 }

      -- Test Safety objective
      let safetyInput = unlines
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
      specSafety <- assertRight $ parseLLIssyFormat safetyInput
      safetyResult <- checkSpecification cvc5Config specSafety
      case safetyResult of
        Left err -> assertFailure $ "CVC5 safety spec check failed: " ++ err
        Right () -> return ()

      -- Test Reachability objective
      let reachInput = unlines
            [ "("
            , "  ((state Int x))"
            , "  ()"
            , "  (("
            , "    ((l0 1 true))"
            , "    ((l0 l0 true))"
            , "    (l0 Reachability)"
            , "  ))"
            , ")"
            ]
      specReach <- assertRight $ parseLLIssyFormat reachInput
      reachResult <- checkSpecification cvc5Config specReach
      case reachResult of
        Left err -> assertFailure $ "CVC5 reachability spec check failed: " ++ err
        Right () -> return ()
  ]
