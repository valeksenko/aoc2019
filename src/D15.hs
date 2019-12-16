module D15 (
    findMin
) where

import IntcodeV3
import Data.List
import Algorithm.Search (dijkstra)

findMin :: [Register] -> Maybe (Int, [State])
findMin = findM . runProgram . mkIntcode 'A' []
    where
        findM = dijkstra unblocked cost found
        found = (==) [2] . take 1 . sOutput
        cost _ _ = 1
        unblocked state = filter ((/=) 0 . head . sOutput) $ map (run state) [1..4]
        run state n = runProgram $ state { sInput = [n] }
