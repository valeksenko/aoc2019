module D04P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D04P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D04P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            290 @=? passwordcount (382345, 843167)
    ]