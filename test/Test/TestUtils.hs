module Test.TestUtils
  ( readFixture
  , requireZ3
  , requireCVC5
  , requireSpot
  , isZ3Available
  , isCVC5Available
  , assertLeft
  , assertRight
  , isLeft
  , isRight
  ) where

import Control.Monad (unless)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty.HUnit (assertFailure)

-- | Read a fixture file relative to project root
readFixture :: FilePath -> IO String
readFixture = readFile

-- | Skip test if Z3 is not available
requireZ3 :: IO ()
requireZ3 = do
  (code, _, _) <- readProcessWithExitCode "z3" ["--version"] ""
  unless (code == ExitSuccess) $
    assertFailure "Z3 not available - skipping test"

-- | Skip test if CVC5 is not available
requireCVC5 :: IO ()
requireCVC5 = do
  (code, _, _) <- readProcessWithExitCode "cvc5" ["--version"] ""
  unless (code == ExitSuccess) $
    assertFailure "CVC5 not available - skipping test"

-- | Check if Z3 is available
isZ3Available :: IO Bool
isZ3Available = do
  (code, _, _) <- readProcessWithExitCode "z3" ["--version"] ""
  return (code == ExitSuccess)

-- | Check if CVC5 is available
isCVC5Available :: IO Bool
isCVC5Available = do
  (code, _, _) <- readProcessWithExitCode "cvc5" ["--version"] ""
  return (code == ExitSuccess)

-- | Skip test if Spot (ltl2tgba) is not available
requireSpot :: IO ()
requireSpot = do
  (code, _, _) <- readProcessWithExitCode "ltl2tgba" ["--version"] ""
  unless (code == ExitSuccess) $
    assertFailure "Spot (ltl2tgba) not available - skipping test"

-- | Assert that result is Left
assertLeft :: Show b => Either a b -> IO a
assertLeft (Left a) = return a
assertLeft (Right b) = assertFailure $ "Expected Left, got Right: " ++ show b

-- | Assert that result is Right
assertRight :: Show a => Either a b -> IO b
assertRight (Right b) = return b
assertRight (Left a) = assertFailure $ "Expected Right, got Left: " ++ show a

-- | Check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Check if Either is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
