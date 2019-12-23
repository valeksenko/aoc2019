module D22P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D22
import qualified Data.Sequence as S

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D22P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            [0,3,6,9,2,5,8,1,4,7] @=? shuffled 10 1 [
                        DealWithIncrement 7, DealIntoNewStack, DealIntoNewStack
                    ]
            [9,2,5,8,1,4,7,0,3,6] @=? shuffled 10 1 [
                        DealIntoNewStack, Cut (-2), DealWithIncrement 7, Cut 8, Cut (-4), DealWithIncrement 7, Cut 3, DealWithIncrement 9, DealWithIncrement 3, Cut (-1)
                    ]
    ]