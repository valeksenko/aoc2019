module D18P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D18P1
import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D18P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            8 @=? stepcount (Explorer {ePosition = (5,1), eMap = M.fromList [((1,1),Key 'b'),((2,1),Passage),((3,1),Door 'A'),((4,1),Passage),((5,1),Passage),((6,1),Passage),((7,1),Key 'a')], eKeys = ""})
            86 @=? stepcount (Explorer {ePosition = (15,1), eMap = M.fromList [((1,1),Key 'f'),((1,3),Key 'd'),((2,1),Passage),((2,3),Passage),((3,1),Door 'D'),((3,3),Passage),((4,1),Passage),((4,3),Passage),((5,1),Door 'E'),((5,3),Passage),((6,1),Passage),((6,3),Passage),((7,1),Key 'e'),((7,3),Passage),((8,1),Passage),((8,3),Passage),((9,1),Door 'C'),((9,3),Passage),((10,1),Passage),((10,3),Passage),((11,1),Key 'b'),((11,3),Passage),((12,1),Passage),((12,3),Passage),((13,1),Door 'A'),((13,3),Passage),((14,1),Passage),((14,3),Passage),((15,1),Passage),((15,3),Passage),((16,1),Passage),((16,3),Passage),((17,1),Key 'a'),((17,3),Passage),((18,1),Passage),((18,3),Passage),((19,1),Door 'B'),((19,3),Passage),((20,1),Passage),((20,3),Passage),((21,1),Key 'c'),((21,3),Passage),((22,1),Passage),((22,2),Passage),((22,3),Passage)], eKeys = ""})
            -- 132 @=? stepcount (Explorer {ePosition = (6,3), eMap = M.fromList [((1,1),Passage),((1,2),Passage),((1,3),Passage),((2,1),Passage),((2,3),Passage),((3,1),Passage),((3,3),Passage),((4,1),Passage),((4,3),Passage),((5,1),Passage),((5,3),Passage),((6,1),Passage),((6,3),Passage),((7,1),Passage),((7,3),Passage),((8,1),Passage),((8,3),Key 'a'),((9,1),Passage),((9,3),Passage),((10,1),Passage),((10,3),Door 'B'),((11,1),Passage),((11,3),Passage),((12,1),Passage),((12,3),Key 'c'),((13,1),Passage),((13,3),Passage),((14,1),Passage),((14,3),Key 'd'),((15,1),Passage),((15,3),Passage),((16,1),Key 'b'),((16,3),Door 'A'),((17,1),Passage),((17,3),Passage),((18,1),Door 'C'),((18,3),Key 'e'),((19,1),Passage),((19,3),Passage),((20,1),Door 'D'),((20,3),Door 'F'),((21,1),Passage),((21,3),Passage),((22,1),Key 'f'),((22,3),Key 'g')], eKeys = ""})
            -- 136 @=? stepcount (Explorer {ePosition = (8,4), eMap = M.fromList [((1,1),Key 'i'),((1,3),Key 'j'),((1,5),Key 'k'),((1,7),Key 'l'),((2,1),Passage),((2,3),Passage),((2,5),Passage),((2,7),Passage),((3,1),Door 'G'),((3,3),Door 'A'),((3,5),Door 'E'),((3,7),Door 'F'),((4,1),Passage),((4,3),Passage),((4,5),Passage),((4,7),Passage),((5,1),Passage),((5,3),Passage),((5,5),Passage),((5,7),Passage),((6,1),Key 'c'),((6,3),Key 'b'),((6,5),Key 'a'),((6,7),Key 'd'),((7,1),Passage),((7,3),Passage),((7,5),Passage),((7,7),Passage),((8,1),Passage),((8,2),Passage),((8,3),Passage),((8,4),Passage),((8,5),Passage),((8,6),Passage),((8,7),Passage),((9,1),Passage),((9,3),Passage),((9,5),Passage),((9,7),Passage),((10,1),Key 'e'),((10,3),Key 'f'),((10,5),Key 'g'),((10,7),Key 'h'),((11,1),Passage),((11,3),Passage),((11,5),Passage),((11,7),Passage),((12,1),Passage),((12,3),Passage),((12,5),Passage),((12,7),Passage),((13,1),Door 'H'),((13,3),Door 'D'),((13,5),Door 'B'),((13,7),Door 'C'),((14,1),Passage),((14,3),Passage),((14,5),Passage),((14,7),Passage),((15,1),Key 'p'),((15,3),Key 'o'),((15,5),Key 'n'),((15,7),Key 'm')], eKeys = ""})
            -- 81 @=? stepcount (Explorer {ePosition = (1,1), eMap = M.fromList [((1,1),Passage),((2,1),Passage),((3,1),Passage),((3,2),Key 'd'),((3,3),Door 'A'),((3,4),Key 'g'),((4,1),Passage),((5,1),Passage),((5,2),Key 'e'),((5,3),Door 'B'),((5,4),Key 'h'),((6,1),Passage),((7,1),Passage),((7,2),Key 'f'),((7,3),Door 'C'),((7,4),Key 'i'),((8,1),Passage),((9,1),Passage),((10,1),Passage),((11,1),Passage),((12,1),Passage),((13,1),Passage),((14,1),Passage),((15,1),Passage),((16,1),Key 'a'),((17,1),Key 'c'),((18,1),Passage),((19,1),Door 'G'),((20,1),Door 'I'),((21,1),Passage),((22,1),Key 'b')], eKeys = ""})
    ]