module D16P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D16P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D16P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            24176176 @=? fftnumber 100 80871224585914546619083218645595    
            73745418 @=? fftnumber 100 19617804207202209144916044189917    
            52432133 @=? fftnumber 100 69317163492948606335995924319873    
    ]