module Test.Unit.Logic.FOLSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set

import Issy.Logic.FOL

tests :: TestTree
tests = testGroup "FOL"
  [ testGroup "Constants"
    [ testCase "true constant" $
        assertEqual "true" true (Const (CBool True))

    , testCase "false constant" $
        assertEqual "false" false (Const (CBool False))

    , testCase "integer constant" $
        assertEqual "int 42" (intConst 42) (Const (CInt 42))

    , testCase "real constant" $
        assertEqual "real 3.14" (realConst 3.14) (Const (CReal 3.14))

    , testCase "zero constant" $
        assertEqual "zero" zeroT (Const (CInt 0))

    , testCase "one constant" $
        assertEqual "one" oneT (Const (CInt 1))
    ]

  , testGroup "Variables"
    [ testCase "boolean variable" $
        assertEqual "bool var" (bvarT "x") (Var "x" SBool)

    , testCase "integer variable" $
        assertEqual "int var" (ivarT "y") (Var "y" SInt)

    , testCase "real variable" $
        assertEqual "real var" (rvarT "z") (Var "z" SReal)

    , testCase "var constructor" $
        assertEqual "var" (var "x" SInt) (Var "x" SInt)
    ]

  , testGroup "Boolean operations - andf"
    [ testCase "andf with false is false" $
        assertEqual "contains false" false (andf [true, false, true])

    , testCase "andf with all true is true" $
        assertEqual "all true" true (andf [true, true])

    , testCase "andf removes true" $ do
        let x = bvarT "x"
        assertEqual "should be x" x (andf [true, x])

    , testCase "andf empty is true" $
        assertEqual "empty and" true (andf [])

    , testCase "andf singleton" $ do
        let x = bvarT "x"
        assertEqual "singleton" x (andf [x])

    , testCase "andf flattens nested ands" $ do
        let x = bvarT "x"
        let y = bvarT "y"
        let z = bvarT "z"
        let nested = andf [x, andf [y, z]]
        -- Should flatten to and(x, y, z)
        case nested of
          Func (PredefF "and") args -> assertEqual "flattened" 3 (length args)
          _ -> assertFailure "Expected and function"
    ]

  , testGroup "Boolean operations - orf"
    [ testCase "orf with true is true" $
        assertEqual "contains true" true (orf [false, true, false])

    , testCase "orf with all false is false" $
        assertEqual "all false" false (orf [false, false])

    , testCase "orf removes false" $ do
        let x = bvarT "x"
        assertEqual "should be x" x (orf [false, x])

    , testCase "orf empty is false" $
        assertEqual "empty or" false (orf [])

    , testCase "orf singleton" $ do
        let x = bvarT "x"
        assertEqual "singleton" x (orf [x])
    ]

  , testGroup "Boolean operations - neg"
    [ testCase "neg true is false" $
        assertEqual "neg true" false (neg true)

    , testCase "neg false is true" $
        assertEqual "neg false" true (neg false)

    , testCase "double negation elimination" $ do
        let x = bvarT "x"
        assertEqual "double neg" x (neg (neg x))
    ]

  , testGroup "Boolean operations - impl"
    [ testCase "impl definition" $ do
        let a = bvarT "a"
        let b = bvarT "b"
        -- impl a b = or(not a, b)
        let result = impl a b
        assertEqual "impl" (orf [neg a, b]) result
    ]

  , testGroup "Comparison operations"
    [ testCase "leqT construction" $ do
        let x = ivarT "x"
        let y = ivarT "y"
        let result = leqT x y
        case result of
          Func (PredefF "<=") [_, _] -> return ()
          _ -> assertFailure "Expected <= function"

    , testCase "geqT construction" $ do
        let result = geqT (ivarT "x") (intConst 0)
        case result of
          Func (PredefF ">=") [_, _] -> return ()
          _ -> assertFailure "Expected >= function"

    , testCase "ltT construction" $ do
        let result = ltT (ivarT "x") (ivarT "y")
        case result of
          Func (PredefF "<") [_, _] -> return ()
          _ -> assertFailure "Expected < function"

    , testCase "gtT construction" $ do
        let result = gtT (ivarT "x") (ivarT "y")
        case result of
          Func (PredefF ">") [_, _] -> return ()
          _ -> assertFailure "Expected > function"

    , testCase "equal construction" $ do
        let result = equal (ivarT "x") (intConst 42)
        case result of
          Func (PredefF "=") [_, _] -> return ()
          _ -> assertFailure "Expected = function"
    ]

  , testGroup "Arithmetic"
    [ testCase "addT empty is zero" $
        assertEqual "empty sum" zeroT (addT [])

    , testCase "addT singleton" $ do
        let x = ivarT "x"
        assertEqual "singleton sum" x (addT [x])

    , testCase "addT multiple" $ do
        let result = addT [ivarT "x", ivarT "y"]
        case result of
          Func (PredefF "+") [_, _] -> return ()
          _ -> assertFailure "Expected + function"
    ]

  , testGroup "Free variables"
    [ testCase "frees of constant is empty" $
        assertEqual "constant has no frees" Set.empty (frees true)

    , testCase "frees of variable" $ do
        let x = ivarT "x"
        assertEqual "should contain x" (Set.singleton "x") (frees x)

    , testCase "frees in function" $ do
        let term = leqT (ivarT "x") (ivarT "y")
        assertEqual "should contain x,y" (Set.fromList ["x", "y"]) (frees term)

    , testCase "frees in nested expression" $ do
        let term = andf [leqT (ivarT "x") (intConst 10), gtT (ivarT "y") zeroT]
        assertEqual "should contain x,y" (Set.fromList ["x", "y"]) (frees term)
    ]

  , testGroup "Quantifiers"
    [ testCase "quantifierFree on constant" $
        assertBool "constant is quantifier free" $ quantifierFree true

    , testCase "quantifierFree on variable" $
        assertBool "variable is quantifier free" $ quantifierFree (ivarT "x")

    , testCase "quantifierFree on function" $
        assertBool "function is quantifier free" $ quantifierFree (leqT (ivarT "x") zeroT)

    , testCase "forAll creates quantifier" $ do
        let term = leqT (ivarT "x") zeroT
        let quantified = forAll ["x"] term
        assertBool "should be quantified" $ not $ quantifierFree quantified

    , testCase "exists creates quantifier" $ do
        let term = leqT (ivarT "x") zeroT
        let quantified = exists ["x"] term
        assertBool "should be quantified" $ not $ quantifierFree quantified
    ]

  , testGroup "NNF conversion"
    [ testCase "toNNF on atom is identity" $ do
        let term = leqT (ivarT "x") zeroT
        assertEqual "atom unchanged" term (toNNF term)

    , testCase "toNNF pushes negation through and" $ do
        let x = bvarT "x"
        let y = bvarT "y"
        let term = neg (andf [x, y])
        let nnf = toNNF term
        -- Should be or(not x, not y)
        case nnf of
          Func (PredefF "or") _ -> return ()
          _ -> assertFailure $ "Expected or, got: " ++ show nnf

    , testCase "toNNF pushes negation through or" $ do
        let x = bvarT "x"
        let y = bvarT "y"
        let term = neg (orf [x, y])
        let nnf = toNNF term
        -- Should be and(not x, not y)
        case nnf of
          Func (PredefF "and") _ -> return ()
          _ -> assertFailure $ "Expected and, got: " ++ show nnf

    , testCase "toNNF eliminates double negation" $ do
        let x = bvarT "x"
        let term = neg (neg x)
        assertEqual "double neg eliminated" x (toNNF term)
    ]

  , testGroup "ITE"
    [ testCase "ite with same branches" $ do
        let x = ivarT "x"
        let c = bvarT "c"
        assertEqual "same branches" x (ite c x x)

    , testCase "ite with different branches" $ do
        let result = ite (bvarT "c") (ivarT "x") (ivarT "y")
        case result of
          Func (PredefF "ite") [_, _, _] -> return ()
          _ -> assertFailure "Expected ite function"
    ]
  ]
