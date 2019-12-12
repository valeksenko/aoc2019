module D12P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D12P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D12P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            179 @=? totalenergy 10 [
                    (-1,0,2), (2,-10,-7),(4,-8,8),(3,5,-1)
                ]
            1940 @=? totalenergy 100 [
                    (-8,-10,0), (5,5,10),(2,-7,3),(9,-8,-3)
                ]
            7202 @=? totalenergy 1000 [
                    (17,-9,4), (2,2,-13),(-1,5,-1),(4,7,-7)
                ]
    ]
