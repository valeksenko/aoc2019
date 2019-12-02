module D01P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D01P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D01P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            2 @=? fuelrequirements 12
            2 @=? fuelrequirements 14
            654 @=? fuelrequirements 1969
            33583 @=? fuelrequirements 100756
    ]