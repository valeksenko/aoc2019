module Main where

import D20P1
import qualified Data.Map as M

main :: IO ()
main = do
    -- f <- readFile "data/d18.txt"
    -- print . stepcount $ parseMap f
    f <- readFile "data/d20.txt"
    print $ stepcount f
