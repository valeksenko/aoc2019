module D15P2 (
    minutes
) where

import D15
import IntcodeV3
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as S
import Debug.Trace

type Distance = Int
type Coordinate = (Int, Int)
type AreaMap = M.Map Coordinate Int


minutes :: [Register] -> Int
minutes =  maximum . M.elems . showMap . buildMap . last . snd . fromJust . findMin
    where
        buildMap state = mapNeighbors (M.singleton (0,0) 0) (getNeighbors 0 (0,0) state $ M.empty)
        mapNeighbors amap [] = amap
        mapNeighbors amap ((distance, pos, state):waitlist) = if M.member pos amap then mapNeighbors amap waitlist else mapNeighbors (M.insert pos distance amap) ((getNeighbors distance pos state amap) ++ waitlist)

getNeighbors :: Distance -> Coordinate -> State -> AreaMap -> [(Distance, Coordinate, State)]
getNeighbors distance (x,y) state amap = notVisited $ freeSpace neighbors
    where
        notVisited = filter (\(_, pos, _) -> M.notMember pos amap) 
        freeSpace = filter (\(_, _, state) -> (==) 1 . head $ sOutput state)
        neighbors = map run [(1, (x,y-1)), (2, (x,y+1)), (3, (x-1,y)), (4, (x+1,y))]
        run (input, pos) = (distance + 1, pos, runProgram $ state { sInput = [input] })

showMap :: AreaMap -> AreaMap
showMap amap = (traceShow $ length showM) amap
    where
        showM = filter ((==) ".") $ map addLine [-35..10]
        addLine y = traceShowId $ map (addPixel y) [-10..40]
        addPixel 0 0 = 'O'
        addPixel y x = if isNothing (M.lookup (x,y) amap) then ' ' else '.'

{-
https://adventofcode.com/2019/day/15#part2

You quickly repair the oxygen system; oxygen gradually fills the area.

Oxygen starts in the location containing the repaired oxygen system. It takes one minute for oxygen to spread to all open locations that are adjacent to a location that already contains oxygen. Diagonal locations are not adjacent.

In the example above, suppose you've used the droid to explore the area fully and have the following map (where locations that currently contain oxygen are marked O):

 ##   
#..## 
#.#..#
#.O.# 
 ###  
Initially, the only location which contains oxygen is the location of the repaired oxygen system. However, after one minute, the oxygen spreads to all open (.) locations that are adjacent to a location containing oxygen:

 ##   
#..## 
#.#..#
#OOO# 
 ###  
After a total of two minutes, the map looks like this:

 ##   
#..## 
#O#O.#
#OOO# 
 ###  
After a total of three minutes:

 ##   
#O.## 
#O#OO#
#OOO# 
 ###  
And finally, the whole region is full of oxygen after a total of four minutes:

 ##   
#OO## 
#O#OO#
#OOO# 
 ###  
So, in this example, all locations contain oxygen after 4 minutes.

Use the repair droid to get a complete map of the area. How many minutes will it take to fill with oxygen?
-}