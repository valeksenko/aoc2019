module IntcodeV2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import IntcodeV2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "IntcodeV1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            [777] @=? exec [777] [1002,8,3,8,3,9,4,9,33,0]
    ]