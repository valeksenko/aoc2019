module D24P2 (
    bugscount
) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map ((!))
import Data.List.Split(divvy)
import Debug.Trace

type Coordinate = (Int, Int)
type AreaMap = M.Map Coordinate Tile
type CompleteMap = M.Map Int AreaMap

data Tile
    = Bug
    | Free
    deriving(Show, Eq, Ord)

bugscount :: Int -> String -> Int
bugscount n = count . last . take (n + 1) . iterate evolve . M.singleton 0 . parseArea
    where
        count = sum . map (M.size . M.filter ((==) Bug)) . M.elems

evolve :: CompleteMap -> CompleteMap
evolve = foldr evolveA M.empty . divvy 3 1 . M.toAscList . expand
    where
        evolveA ((_, a0):(k, a1):(_, a2):_) = M.insert k (evolveArea a0 a1 a2)

expand :: CompleteMap -> CompleteMap
expand cmap = addArea (minLevel - 2) . addArea (minLevel - 1) . addArea (maxLevel + 1) $ addArea (maxLevel + 2) cmap
    where
        addArea l = M.insert l emptyArea
        emptyArea = M.fromList [ ((x, y), Free) | x <- [0..4], y <- [0..4] ]
        minLevel = minimum $ M.keys cmap
        maxLevel = maximum $ M.keys cmap

evolveArea :: AreaMap -> AreaMap -> AreaMap -> AreaMap
evolveArea a0 a1 a2 = M.foldrWithKey addTile M.empty a1
    where
        addTile c tile = M.insert c (newTile c tile)
        newTile (2,2) _ = Free
        newTile c tile = forTile tile . sum $ map (neighborBugs a0 a1 a2 c) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        forTile Bug n = if n == 1 then Bug else Free
        forTile Free n = if (n == 1) || (n == 2) then Bug else Free

neighborBugs :: AreaMap -> AreaMap -> AreaMap -> Coordinate -> Coordinate -> Int
neighborBugs a0 a1 a2 (x,y) (x',y') = length . filter ((==) Bug) $ tiles neighbors
    where
        tiles (amap, cc) = map ((!) amap) cc
        neighbors = case (x + x', y + y') of
            (-1, _) -> (a2, [(1, 2)])
            (5, _) -> (a2, [(3, 2)])
            (_, -1) -> (a2, [(2, 1)])
            (_, 5) -> (a2, [(2, 3)])
            (2, 2) -> (a0, nextArea)
            c -> (a1, [c])
        nextArea = case (x', y') of
            (1, _) -> [ (0, i) | i <- [0..4] ]
            (-1, _) -> [ (4, i) | i <- [0..4] ]
            (_, 1) -> [ (i, 0) | i <- [0..4] ]
            (_, -1) -> [ (i, 4) | i <- [0..4] ]

parseArea :: String -> AreaMap
parseArea = fst . foldl' parse (M.empty, (0,0))
    where
        parse (amap, (y, x)) c = case c of
            '.' -> (M.insert (x, y) Free amap, (y, x + 1))
            '#' -> (M.insert (x, y) Bug amap, (y, x + 1))
            '\n' -> (amap, (y + 1, 0))
            otherwise -> (amap, (y, x + 1))

{-
https://adventofcode.com/2019/day/24#part2

After careful analysis, one thing is certain: you have no idea where all these bugs are coming from.

Then, you remember: Eris is an old Plutonian settlement! Clearly, the bugs are coming from recursively-folded space.

This 5x5 grid is only one level in an infinite number of recursion levels. The tile in the middle of the grid is actually another 5x5 grid, the grid in your scan is contained as the middle tile of a larger 5x5 grid, and so on. Two levels of grids look like this:

     |     |         |     |     
     |     |         |     |     
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
     |     |         |     |     
     |     |         |     |     
-----+-----+---------+-----+-----
     |     | | | | | |     |     
     |     |-+-+-+-+-|     |     
     |     | | | | | |     |     
     |     |-+-+-+-+-|     |     
     |     | | |?| | |     |     
     |     |-+-+-+-+-|     |     
     |     | | | | | |     |     
     |     |-+-+-+-+-|     |     
     |     | | | | | |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
     |     |         |     |     
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
     |     |         |     |     
     |     |         |     |     
(To save space, some of the tiles are not drawn to scale.) Remember, this is only a small part of the infinitely recursive grid; there is a 5x5 grid that contains this diagram, and a 5x5 grid that contains that one, and so on. Also, the ? in the diagram contains another 5x5 grid, which itself contains another 5x5 grid, and so on.

The scan you took (your puzzle input) shows where the bugs are on a single level of this structure. The middle tile of your scan is empty to accommodate the recursive grids within it. Initially, no other levels contain bugs.

Tiles still count as adjacent if they are directly up, down, left, or right of a given tile. Some tiles have adjacent tiles at a recursion level above or below its own level. For example:

     |     |         |     |     
  1  |  2  |    3    |  4  |  5  
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
  6  |  7  |    8    |  9  |  10 
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |A|B|C|D|E|     |     
     |     |-+-+-+-+-|     |     
     |     |F|G|H|I|J|     |     
     |     |-+-+-+-+-|     |     
 11  | 12  |K|L|?|N|O|  14 |  15 
     |     |-+-+-+-+-|     |     
     |     |P|Q|R|S|T|     |     
     |     |-+-+-+-+-|     |     
     |     |U|V|W|X|Y|     |     
-----+-----+---------+-----+-----
     |     |         |     |     
 16  | 17  |    18   |  19 |  20 
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
 21  | 22  |    23   |  24 |  25 
     |     |         |     |     
Tile 19 has four adjacent tiles: 14, 18, 20, and 24.
Tile G has four adjacent tiles: B, F, H, and L.
Tile D has four adjacent tiles: 8, C, E, and I.
Tile E has four adjacent tiles: 8, D, 14, and J.
Tile 14 has eight adjacent tiles: 9, E, J, O, T, Y, 15, and 19.
Tile N has eight adjacent tiles: I, O, S, and five tiles within the sub-grid marked ?.
The rules about bugs living and dying are the same as before.

For example, consider the same initial state as above:

....#
#..#.
#.?##
..#..
#....
The center tile is drawn as ? to indicate the next recursive grid. Call this level 0; the grid within this one is level 1, and the grid that contains this one is level -1. Then, after ten minutes, the grid at each level would look like this:

Depth -5:
..#..
.#.#.
..?.#
.#.#.
..#..

Depth -4:
...#.
...##
..?..
...##
...#.

Depth -3:
#.#..
.#...
..?..
.#...
#.#..

Depth -2:
.#.##
....#
..?.#
...##
.###.

Depth -1:
#..##
...##
..?..
...#.
.####

Depth 0:
.#...
.#.##
.#?..
.....
.....

Depth 1:
.##..
#..##
..?.#
##.##
#####

Depth 2:
###..
##.#.
#.?..
.#.##
#.#..

Depth 3:
..###
.....
#.?..
#....
#...#

Depth 4:
.###.
#..#.
#.?..
##.#.
.....

Depth 5:
####.
#..#.
#.?#.
####.
.....
In this example, after 10 minutes, a total of 99 bugs are present.

Starting with your scan, how many bugs are present after 200 minutes?
-}