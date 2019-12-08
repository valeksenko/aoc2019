module D08P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D08P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D08P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            "0110" @=? decodeimage 2 2 "0222112222120000"
    ]