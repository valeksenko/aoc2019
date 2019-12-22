module D20P2 (
    stepcount
) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map ((!))
import Algorithm.Search (dijkstra)

type Coordinate = (Int, Int)
type Level = Int
type PortalMap = M.Map Portal Portal

data Portal
    = In Coordinate
    | Out Coordinate
    deriving(Show, Eq, Ord)

data Explorer =
    Explorer {
        ePosition :: (Level, Coordinate)
      , eExit :: (Level, Coordinate) 
      , ePassages :: [Coordinate]
      , ePortalMap :: PortalMap
    } deriving(Show, Eq, Ord)

stepcount = 1
-- stepcount :: String -> Int
-- stepcount = fst . fromJust . steps . mkExplorer
--     where
--         steps = dijkstra neighbors cost exited
--         exited e = (ePosition e) == (eExit e)
--         cost _ _ = 1
--         neighbors e = map (explorer e) . catMaybes . (:) (portal e) $ map (neighbor (ePosition e) (ePassages e)) [(0, 1), (0, -1), (1, 0), (-1, 0)]
--         explorer e pos = e { ePosition = pos }        
--         portal e = M.lookup (ePosition e) $ ePortalMap e
--         neighbor (x, y) passages (x', y') = find ((==) (x + x', y + y')) passages 

-- mkExplorer :: String -> Explorer
-- mkExplorer  = newExplorer . parseMaze
--     where
--         newExplorer (passages, portals) = newE passages $ mapPortals passages portals
--         newE passages portals = Explorer (0, findPortal "AA" portals) (0, findPortal "ZZ" portals) passages (portalPairs portals)
--         findPortal pid = getPost . head . M.keys . M.filter ((==) pid)
--         portalPairs portals = foldr (pPair portals) M.empty $ M.toList portals
--         pPair portals (pos, name) pairs = mkPair pos pairs . filter ((/=) pos) . M.keys $ M.filter ((==) name) portals
--         mkPair pos pairs other = if null other then pairs else M.insert pos (head other) pairs
--         getPos (In p) = p
--         getPos (Out p) = p

-- mapPortals :: [Coordinate] -> M.Map Coordinate Char -> M.Map Portal String
-- mapPortals passages portals = foldr (addPortal portals) M.empty passages
--     where
--        addPortal portals pos pmap = foldr (addP pos) pmap . catMaybes $ map (neighbor portals pos) [(0, 1), (0, -1), (1, 0), (-1, 0)]
--        addP pos (ptype, pid) = M.insert (ptype pos) pid
--        neighbor portals (x, y) (x', y') = (M.lookup (x + x', y + y') portals) >>= (Just . newPortal (x', y') portals (x + x' + x', y + y' + y'))
--        newPortal p@(x, y) portals pos char = surface p pos $ if (x + y) > 0 then [char, portals ! pos] else [portals ! pos, char]
--        surface p pos = if any (outside p pos) passages then IN else OUT
--        outside (x', y') (x1, y1) (x2, y2) = 

-- parseMaze :: String -> ([Coordinate], M.Map Coordinate Char)
-- parseMaze = fst . foldl' parse (([], M.empty), (0,0))
--     where
--         parse (m@(passages, portals), (y, x)) c = case c of
--             '.' -> (((x, y):passages, portals), (y, x + 1))
--             '#' -> (m, (y, x + 1))
--             ' ' -> (m, (y, x + 1))
--             '\n' -> (m, (y + 1, 0))
--             otherwise -> ((passages, M.insert (x, y) c portals), (y, x + 1))


{-
https://adventofcode.com/2019/day/20#part2

Strangely, the exit isn't open when you reach it. Then, you remember: the ancient Plutonians were famous for building recursive spaces.

The marked connections in the maze aren't portals: they physically connect to a larger or smaller copy of the maze. Specifically, the labeled tiles around the inside edge actually connect to a smaller copy of the same maze, and the smaller copy's inner labeled tiles connect to yet a smaller copy, and so on.

When you enter the maze, you are at the outermost level; when at the outermost level, only the outer labels AA and ZZ function (as the start and end, respectively); all other outer labeled tiles are effectively walls. At any other level, AA and ZZ count as walls, but the other outer labeled tiles bring you one level outward.

Your goal is to find a path through the maze that brings you back to ZZ at the outermost level of the maze.

In the first example above, the shortest path is now the loop around the right side. If the starting level is 0, then taking the previously-shortest path would pass through BC (to level 1), DE (to level 2), and FG (back to level 1). Because this is not the outermost level, ZZ is a wall, and the only option is to go back around to BC, which would only send you even deeper into the recursive maze.

In the second example above, there is no path that brings you to ZZ at the outermost level.

Here is a more interesting example:

             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     
One shortest path through the maze is the following:

Walk from AA to XF (16 steps)
Recurse into level 1 through XF (1 step)
Walk from XF to CK (10 steps)
Recurse into level 2 through CK (1 step)
Walk from CK to ZH (14 steps)
Recurse into level 3 through ZH (1 step)
Walk from ZH to WB (10 steps)
Recurse into level 4 through WB (1 step)
Walk from WB to IC (10 steps)
Recurse into level 5 through IC (1 step)
Walk from IC to RF (10 steps)
Recurse into level 6 through RF (1 step)
Walk from RF to NM (8 steps)
Recurse into level 7 through NM (1 step)
Walk from NM to LP (12 steps)
Recurse into level 8 through LP (1 step)
Walk from LP to FD (24 steps)
Recurse into level 9 through FD (1 step)
Walk from FD to XQ (8 steps)
Recurse into level 10 through XQ (1 step)
Walk from XQ to WB (4 steps)
Return to level 9 through WB (1 step)
Walk from WB to ZH (10 steps)
Return to level 8 through ZH (1 step)
Walk from ZH to CK (14 steps)
Return to level 7 through CK (1 step)
Walk from CK to XF (10 steps)
Return to level 6 through XF (1 step)
Walk from XF to OA (14 steps)
Return to level 5 through OA (1 step)
Walk from OA to CJ (8 steps)
Return to level 4 through CJ (1 step)
Walk from CJ to RE (8 steps)
Return to level 3 through RE (1 step)
Walk from RE to IC (4 steps)
Recurse into level 4 through IC (1 step)
Walk from IC to RF (10 steps)
Recurse into level 5 through RF (1 step)
Walk from RF to NM (8 steps)
Recurse into level 6 through NM (1 step)
Walk from NM to LP (12 steps)
Recurse into level 7 through LP (1 step)
Walk from LP to FD (24 steps)
Recurse into level 8 through FD (1 step)
Walk from FD to XQ (8 steps)
Recurse into level 9 through XQ (1 step)
Walk from XQ to WB (4 steps)
Return to level 8 through WB (1 step)
Walk from WB to ZH (10 steps)
Return to level 7 through ZH (1 step)
Walk from ZH to CK (14 steps)
Return to level 6 through CK (1 step)
Walk from CK to XF (10 steps)
Return to level 5 through XF (1 step)
Walk from XF to OA (14 steps)
Return to level 4 through OA (1 step)
Walk from OA to CJ (8 steps)
Return to level 3 through CJ (1 step)
Walk from CJ to RE (8 steps)
Return to level 2 through RE (1 step)
Walk from RE to XQ (14 steps)
Return to level 1 through XQ (1 step)
Walk from XQ to FD (8 steps)
Return to level 0 through FD (1 step)
Walk from FD to ZZ (18 steps)
This path takes a total of 396 steps to move from AA at the outermost layer to ZZ at the outermost layer.

In your maze, when accounting for recursion, how many steps does it take to get from the open tile marked AA to the open tile marked ZZ, both at the outermost layer?
-}