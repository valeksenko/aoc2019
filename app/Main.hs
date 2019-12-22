module Main where

import D22P1
import qualified Data.Map as M

main :: IO ()
main = do
    -- f <- readFile "data/d18.txt"
    -- print . stepcount $ parseMap f

    print $ cardposition 10007 [
            Cut 8988, DealWithIncrement 25, Cut (-797), DealWithIncrement 26, DealIntoNewStack, Cut (-2168), DealWithIncrement 39, Cut (-602), DealWithIncrement 67, Cut 1314, DealIntoNewStack, DealWithIncrement 34, Cut (-5388), DealWithIncrement 69, DealIntoNewStack, DealWithIncrement 38, Cut 579, DealIntoNewStack, DealWithIncrement 4, DealIntoNewStack, DealWithIncrement 32, Cut 9476, DealWithIncrement 71, Cut (-9536), DealIntoNewStack, Cut 739, DealWithIncrement 19, Cut 8867, DealIntoNewStack, DealWithIncrement 26, Cut 1596, DealWithIncrement 45, Cut (-610), DealWithIncrement 49, Cut (-8781), DealWithIncrement 68, Cut (-9322), DealIntoNewStack, DealWithIncrement 41, Cut (-557), DealIntoNewStack, DealWithIncrement 25, Cut 9649, DealWithIncrement 58, Cut (-8004), DealWithIncrement 70, Cut (-8938), DealWithIncrement 24, Cut 1409, DealWithIncrement 33, Cut (-2854), DealWithIncrement 4, Cut (-2098), DealWithIncrement 44, Cut 6281, DealWithIncrement 13, Cut (-2661), DealWithIncrement 32, Cut (-8103), DealWithIncrement 5, DealIntoNewStack, Cut (-6363), DealWithIncrement 47, Cut 8796, DealWithIncrement 61, DealIntoNewStack, Cut (-6258), DealWithIncrement 35, Cut 1920, DealWithIncrement 31, Cut 4034, DealWithIncrement 53, Cut 6002, DealWithIncrement 51, Cut 4284, DealWithIncrement 22, DealIntoNewStack, Cut (-1707), DealWithIncrement 67, Cut 83, DealWithIncrement 73, Cut 6809, DealWithIncrement 70, Cut (-3948), DealWithIncrement 66, Cut (-5498), DealWithIncrement 3, Cut (-5322), DealWithIncrement 52, Cut (-4069), DealWithIncrement 25, Cut (-7756), DealWithIncrement 34, Cut (-7383), DealWithIncrement 38, Cut 2544, DealWithIncrement 23, Cut (-1937), DealWithIncrement 16, Cut (-2114)
        ]
