module Main where

import D12P1

main :: IO ()
main = do
    print $ totalenergy 1000 [
            (17,-9,4), (2,2,-13),(-1,5,-1),(4,7,-7)
        ]
