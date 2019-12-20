module D20P1 (
    stepcount
) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map ((!))
import Algorithm.Search (dijkstra)

type Coordinate = (Int, Int)
type PortalMap = M.Map Coordinate Coordinate

data Explorer =
    Explorer {
        ePosition :: Coordinate
      , eExit :: Coordinate 
      , ePassages :: [Coordinate]
      , ePortalMap :: PortalMap
    } deriving(Show, Eq, Ord)

stepcount :: String -> Int
stepcount = fst . fromJust . steps . mkExplorer
    where
        steps = dijkstra neighbors cost exited
        exited e = (ePosition e) == (eExit e)
        cost _ _ = 1
        neighbors e = map (explorer e) . catMaybes . (:) (portal e) $ map (neighbor (ePosition e) (ePassages e)) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        explorer e pos = e { ePosition = pos }        
        portal e = M.lookup (ePosition e) $ ePortalMap e
        neighbor (x, y) passages (x', y') = find ((==) (x + x', y + y')) passages 

mkExplorer :: String -> Explorer
mkExplorer  = newExplorer . parseMaze
    where
        newExplorer (passages, portals) = newE passages $ mapPortals passages portals
        newE passages portals = Explorer (findPortal "AA" portals) (findPortal "ZZ" portals) passages (portalPairs portals)
        findPortal name = head . M.keys . M.filter ((==) name)
        portalPairs portals = foldr (pPair portals) M.empty $ M.toList portals
        pPair portals (pos, name) pairs = mkPair pos pairs . filter ((/=) pos) . M.keys $ M.filter ((==) name) portals
        mkPair pos pairs other = if null other then pairs else M.insert pos (head other) pairs

mapPortals :: [Coordinate] -> M.Map Coordinate Char -> M.Map Coordinate String
mapPortals passages portals = foldr (addPortal portals) M.empty passages
    where
       addPortal portals pos pmap = foldr (addP pos) pmap . catMaybes $ map (neighbor portals pos) [(0, 1), (0, -1), (1, 0), (-1, 0)]
       addP pos portal = M.insert pos portal
       neighbor portals (x, y) (x', y') = (M.lookup (x + x', y + y') portals) >>= (Just . (newPortal (x' + y')) portals (x + x' + x', y + y' + y'))
       newPortal ord portals pos char = if ord > 0 then [char, portals ! pos] else [portals ! pos, char]

parseMaze :: String -> ([Coordinate], M.Map Coordinate Char)
parseMaze = fst . foldl' parse (([], M.empty), (0,0))
    where
        parse (m@(passages, portals), (y, x)) c = case c of
            '.' -> (((x, y):passages, portals), (y, x + 1))
            '#' -> (m, (y, x + 1))
            ' ' -> (m, (y, x + 1))
            '\n' -> (m, (y + 1, 0))
            otherwise -> ((passages, M.insert (x, y) c portals), (y, x + 1))


{-
https://adventofcode.com/2019/day/20

You notice a strange pattern on the surface of Pluto and land nearby to get a closer look. Upon closer inspection, you realize you've come across one of the famous space-warping mazes of the long-lost Pluto civilization!

Because there isn't much space on Pluto, the civilization that used to live here thrived by inventing a method for folding spacetime. Although the technology is no longer understood, mazes like this one provide a small glimpse into the daily life of an ancient Pluto citizen.

This maze is shaped like a donut. Portals along the inner and outer edge of the donut can instantly teleport you from one side to the other. For example:

         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       
This map of the maze shows solid walls (#) and open passages (.). Every maze on Pluto has a start (the open tile next to AA) and an end (the open tile next to ZZ). Mazes on Pluto also have portals; this maze has three pairs of portals: BC, DE, and FG. When on an open tile next to one of these labels, a single step can take you to the other tile with the same label. (You can only walk on . tiles; labels and empty space are not traversable.)

One path through the maze doesn't require any portals. Starting at AA, you could go down 1, right 8, down 12, left 4, and down 1 to reach ZZ, a total of 26 steps.

However, there is a shorter path: You could walk from AA to the inner BC portal (4 steps), warp to the outer BC portal (1 step), walk to the inner DE (6 steps), warp to the outer DE (1 step), walk to the outer FG (4 steps), warp to the inner FG (1 step), and finally walk to ZZ (6 steps). In total, this is only 23 steps.

Here is a larger example:

                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               
Here, AA has no direct path to ZZ, but it does connect to AS and CP. By passing through AS, QG, BU, and JO, you can reach ZZ in 58 steps.

In your maze, how many steps does it take to get from the open tile marked AA to the open tile marked ZZ?
-}