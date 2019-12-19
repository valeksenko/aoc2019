module Main where

import D18P1
import qualified Data.Map as M

main :: IO ()
main = do
    -- f <- readFile "data/d18.txt"
    -- print . stepcount $ parseMap f
    print $ stepcount (Explorer {ePosition = (8,4), eMap = M.fromList [((1,1),Key 'i'),((1,3),Key 'j'),((1,5),Key 'k'),((1,7),Key 'l'),((2,1),Passage),((2,3),Passage),((2,5),Passage),((2,7),Passage),((3,1),Door 'G'),((3,3),Door 'A'),((3,5),Door 'E'),((3,7),Door 'F'),((4,1),Passage),((4,3),Passage),((4,5),Passage),((4,7),Passage),((5,1),Passage),((5,3),Passage),((5,5),Passage),((5,7),Passage),((6,1),Key 'c'),((6,3),Key 'b'),((6,5),Key 'a'),((6,7),Key 'd'),((7,1),Passage),((7,3),Passage),((7,5),Passage),((7,7),Passage),((8,1),Passage),((8,2),Passage),((8,3),Passage),((8,4),Passage),((8,5),Passage),((8,6),Passage),((8,7),Passage),((9,1),Passage),((9,3),Passage),((9,5),Passage),((9,7),Passage),((10,1),Key 'e'),((10,3),Key 'f'),((10,5),Key 'g'),((10,7),Key 'h'),((11,1),Passage),((11,3),Passage),((11,5),Passage),((11,7),Passage),((12,1),Passage),((12,3),Passage),((12,5),Passage),((12,7),Passage),((13,1),Door 'H'),((13,3),Door 'D'),((13,5),Door 'B'),((13,7),Door 'C'),((14,1),Passage),((14,3),Passage),((14,5),Passage),((14,7),Passage),((15,1),Key 'p'),((15,3),Key 'o'),((15,5),Key 'n'),((15,7),Key 'm')], eKeys = ""})
            