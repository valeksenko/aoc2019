module Main where

import D10
import D10P2

main :: IO ()
main = do
    f <- readFile "data/d10.txt"
    print $ let
            spaceMap = parseSpaceMap f
        in stationCoordinate spaceMap