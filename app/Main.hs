module Main where

import D14P1

main :: IO ()
main = do
    -- f <- readFile "data/d14.txt"
    f <- readFile "1"
    print $ parseReactions f
    print . orecount $ parseReactions f
    -- print $ parseReactions f
    -- print $ orecount [
    --                 (("A",10),[("ORE",10)]),(("B",1),[("ORE",1)]),(("C",1),[("A",7),("B",1)]),(("D",1),[("A",7),("C",1)]),(("E",1),[("A",7),("D",1)]),(("FUEL",1),[("A",7),("E",1)])
    --             ]