module Test.Unit.Compiler.LexerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Compiler.Lexer (tokenize)
import Compiler.Base (Token(..), tval)
import Test.TestUtils (isLeft, isRight)

tests :: TestTree
tests = testGroup "Lexer"
  [ testGroup "Basic tokenization"
    [ testCase "tokenize simple identifiers" $ do
        let Right tokens = tokenize "input int x"
        assertEqual "token count" 3 (length tokens)
        assertEqual "first token" "input" (tval $ head tokens)
        assertEqual "second token" "int" (tval $ tokens !! 1)
        assertEqual "third token" "x" (tval $ tokens !! 2)

    , testCase "tokenize keywords" $ do
        let Right tokens = tokenize "formula assert assume"
        assertEqual "token count" 3 (length tokens)
        assertEqual "keywords" ["formula", "assert", "assume"] (map tval tokens)

    , testCase "tokenize operators" $ do
        let Right tokens = tokenize "+ - * / = < > <= >="
        assertEqual "operators" ["+", "-", "*", "/", "=", "<", ">", "<=", ">="] (map tval tokens)

    , testCase "tokenize single characters" $ do
        let Right tokens = tokenize "()[]{}"
        assertEqual "brackets" ["(", ")", "[", "]", "{", "}"] (map tval tokens)
    ]

  , testGroup "Numbers"
    [ testCase "tokenize integers" $ do
        let Right tokens = tokenize "42 0 123"
        assertEqual "integers" ["42", "0", "123"] (map tval tokens)

    , testCase "tokenize real numbers" $ do
        let Right tokens = tokenize "3.14 0.5"
        assertEqual "reals" ["3.14", "0.5"] (map tval tokens)
    ]

  , testGroup "Comments"
    [ testCase "skip single-line comments" $ do
        let Right tokens = tokenize "input // this is a comment\nint"
        assertEqual "should skip comment" ["input", "int"] (map tval tokens)

    , testCase "skip multi-line comments" $ do
        let Right tokens = tokenize "input /* multi\nline\ncomment */ int"
        assertEqual "should skip multiline" ["input", "int"] (map tval tokens)

    , testCase "handle empty single-line comment" $ do
        let Right tokens = tokenize "input //\nint"
        assertEqual "empty comment" ["input", "int"] (map tval tokens)

    , testCase "handle comment at end of file" $ do
        let Right tokens = tokenize "input // comment"
        assertEqual "trailing comment" ["input"] (map tval tokens)
    ]

  , testGroup "Whitespace handling"
    [ testCase "handle multiple spaces" $ do
        let Right tokens = tokenize "input    int"
        assertEqual "multiple spaces" ["input", "int"] (map tval tokens)

    , testCase "handle tabs" $ do
        let Right tokens = tokenize "input\tint"
        assertEqual "tabs" ["input", "int"] (map tval tokens)

    , testCase "handle mixed whitespace" $ do
        let Right tokens = tokenize "  input  \t\n  int  "
        assertEqual "mixed whitespace" ["input", "int"] (map tval tokens)
    ]

  , testGroup "Line endings"
    [ testCase "handle Unix line endings (LF)" $ do
        let Right tokens = tokenize "a\nb"
        assertEqual "unix" ["a", "b"] (map tval tokens)

    , testCase "handle Windows line endings (CRLF)" $ do
        let Right tokens = tokenize "a\r\nb"
        assertEqual "windows" ["a", "b"] (map tval tokens)

    , testCase "handle old Mac line endings (CR)" $ do
        let Right tokens = tokenize "a\rb"
        assertEqual "mac" ["a", "b"] (map tval tokens)
    ]

  , testGroup "Edge cases"
    [ testCase "empty input" $ do
        let Right tokens = tokenize ""
        assertEqual "empty produces empty" [] tokens

    , testCase "whitespace only" $ do
        let Right tokens = tokenize "   \n\t  "
        assertEqual "whitespace only" [] tokens

    , testCase "reject illegal characters" $ do
        assertBool "should reject @" $ isLeft $ tokenize "input @"
        assertBool "should reject $" $ isLeft $ tokenize "$var"
        assertBool "should reject #" $ isLeft $ tokenize "# comment"

    , testCase "unclosed multi-line comment" $ do
        assertBool "should reject unclosed comment" $ isLeft $ tokenize "input /* unclosed"
    ]

  , testGroup "Complex inputs"
    [ testCase "tokenize formula expression" $ do
        let Right tokens = tokenize "formula { assert F [x > 0] }"
        assertBool "should have tokens" $ length tokens > 5

    , testCase "tokenize game definition" $ do
        let Right tokens = tokenize "game Safety from l0 { loc l0 1 }"
        let values = map tval tokens
        assertBool "contains game" $ "game" `elem` values
        assertBool "contains Safety" $ "Safety" `elem` values
        assertBool "contains l0" $ "l0" `elem` values

    , testCase "tokenize arithmetic expression" $ do
        let Right tokens = tokenize "[x' = x + i]"
        let values = map tval tokens
        assertBool "contains =" $ "=" `elem` values
        assertBool "contains +" $ "+" `elem` values
    ]
  ]
