module Main (main) where

import Test.Tasty

-- Integration tests
import qualified Test.Integration.CompileSpec
import qualified Test.Integration.ParsePrintSpec
import qualified Test.Integration.SolveSpec

-- Unit tests
import qualified Test.Unit.Compiler.LexerSpec
import qualified Test.Unit.Logic.FOLSpec
import qualified Test.Unit.Parsers.LLIssyFormatSpec

main :: IO ()
main = defaultMain $ testGroup "Issy Tests"
  [ testGroup "Integration Tests"
    [ Test.Integration.CompileSpec.tests
    , Test.Integration.ParsePrintSpec.tests
    , Test.Integration.SolveSpec.tests
    ]
  , testGroup "Unit Tests"
    [ testGroup "Compiler"
      [ Test.Unit.Compiler.LexerSpec.tests
      ]
    , testGroup "Logic"
      [ Test.Unit.Logic.FOLSpec.tests
      ]
    , testGroup "Parsers"
      [ Test.Unit.Parsers.LLIssyFormatSpec.tests
      ]
    ]
  ]
