module D09P2 (
    distresscoordinates
) where

import IntcodeV3
import Data.List

distresscoordinates :: [Register] -> Int
distresscoordinates = head . exec [2]

{-
https://adventofcode.com/2019/day/9#part2

Finally, you can lock on to the Ceres distress signal! You just need to boost your sensors using the BOOST program.

The program runs in sensor boost mode by providing the input instruction the value 2. Once run, it will boost the sensors automatically, but it might take a few seconds to complete the operation on slower hardware. In sensor boost mode, the program will output a single value: the coordinates of the distress signal.

Run the BOOST program in sensor boost mode. What are the coordinates of the distress signal?
-}