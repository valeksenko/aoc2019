module Main where

import D10P1

main :: IO ()
main = do
    f <- readFile "data/d10.txt"
    print $ let
            spaceMap = parseSpaceMap f
        in maxasteroidcount spaceMap