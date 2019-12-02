module D02Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D02

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            [3500,9,10,70,2,3,11,0,99,30,40,50] @=? exec [1,9,10,3,2,3,11,0,99,30,40,50]
            [2,0,0,0,99] @=? exec [1,0,0,0,99]
            [2,3,0,6,99] @=? exec [2,3,0,3,99]
            [2,4,4,5,99,9801] @=? exec [2,4,4,5,99,0]
            [30,1,1,4,2,5,6,0,99] @=? exec [1,1,1,4,99,5,6,0,99]
    ]