module D04P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D04P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D04P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            460 @=? passwordcount (382345, 843167)
    ]