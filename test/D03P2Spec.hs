module D03P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D03P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D03P2 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            30 @=? stepstointersection [R 8,U 5,L 5,D 3] [U 7,R 6,D 4,L 4]
            610 @=? stepstointersection [R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72] [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]
            410 @=? stepstointersection [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51] [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]
    ]