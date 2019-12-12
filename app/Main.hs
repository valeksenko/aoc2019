module Main where

import D12P2

main :: IO ()
main = do
    print $ stepcount [
            (17,-9,4), (2,2,-13),(-1,5,-1),(4,7,-7)
        ]
