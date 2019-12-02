module D01P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D01P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D01P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            2 @=? fullfuelrequirements 14
            966 @=? fullfuelrequirements 1969
            50346 @=? fullfuelrequirements 100756
    ]