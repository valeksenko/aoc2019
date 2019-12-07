module D06P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D06P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D06P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            4 @=? mintransfers [
                    "COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"
                ]
    ]
