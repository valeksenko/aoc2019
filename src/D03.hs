module D03 (
    toCoordinates
  , intersection    
  , Coordinate    
  , Path(..)    
) where

import Data.List
import Data.List.Ordered

type Coordinate = (Int, Int)

data Path
    = R Int
    | L Int
    | U Int
    | D Int
    deriving(Show, Eq)

port = (0, 0)

toCoordinates :: [Path] -> [Coordinate]
toCoordinates = tail . reverse . foldl' toC [port] 
    where
        toC coordinates@(c:cs) path = (pathC path c) ++ coordinates
        pathC path (x, y) = case path of
            R l -> [(x+i, y) | i <- reverse [1..l]]
            L l -> [(x-i, y) | i <- reverse [1..l]]
            U l -> [(x, y-i) | i <- reverse [1..l]]
            D l -> [(x, y+i) | i <- reverse [1..l]]

intersection :: [Coordinate] -> [Coordinate] -> [Coordinate]
intersection c1 c2 = isect (sort c1) (sort c2)

{-
https://adventofcode.com/2019/day/3

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........
Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........
These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
What is the Manhattan distance from the central port to the closest intersection?
-}