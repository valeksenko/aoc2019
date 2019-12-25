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
https://adventofcode.com/2019/day/24

You land on Eris, your last stop before reaching Santa. As soon as you do, your sensors start picking up strange life forms moving around: Eris is infested with bugs! With an over 24-hour roundtrip for messages between you and Earth, you'll have to deal with this problem on your own.

Eris isn't a very large place; a scan of the entire area fits into a 5x5 grid (your puzzle input). The scan shows bugs (#) and empty spaces (.).

Each minute, The bugs live and die based on the number of bugs in the four adjacent tiles:

A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
Otherwise, a bug or empty space remains the same. (Tiles on the edges of the grid have fewer than four adjacent tiles; the missing tiles count as empty space.) This process happens in every location simultaneously; that is, within the same minute, the number of adjacent bugs is counted for every tile first, and then the tiles are updated.

Here are the first few minutes of an example scenario:

Initial state:
....#
#..#.
#..##
..#..
#....

After 1 minute:
#..#.
####.
###.#
##.##
.##..

After 2 minutes:
#####
....#
....#
...#.
#.###

After 3 minutes:
#....
####.
...##
#.##.
.##.#

After 4 minutes:
####.
....#
##..#
.....
##...
To understand the nature of the bugs, watch for the first time a layout of bugs and empty spaces matches any previous layout. In the example above, the first layout to appear twice is:

.....
.....
.....
#....
.#...
To calculate the biodiversity rating for this layout, consider each tile left-to-right in the top row, then left-to-right in the second row, and so on. Each of these tiles is worth biodiversity points equal to increasing powers of two: 1, 2, 4, 8, 16, 32, and so on. Add up the biodiversity points for tiles with bugs; in this example, the 16th tile (32768 points) and 22nd tile (2097152 points) have bugs, a total biodiversity rating of 2129920.

What is the biodiversity rating for the first layout that appears twice?
-}