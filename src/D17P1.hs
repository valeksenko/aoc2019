module D17P1 (
    calibrationcode
  , showMap
) where

import IntcodeV3
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace

type Coordinate = (Int, Int)
type PixelMap = M.Map Coordinate Pixel

data Direction
    = North
    | South
    | West
    | East
    | Tumbling
    deriving(Show, Eq)

data Pixel
    = Scaffold
    | Space
    | Robot Direction
    deriving(Show, Eq)

scaffold = 35
space = 46

calibrationcode :: [Register] -> Int
calibrationcode =  sum . alignmentParams . buildMap . exec []

alignmentParams :: PixelMap -> [Int]
alignmentParams vmap = map (uncurry (*)) . map distances . M.keys $ M.filterWithKey intersection vmap
    where
        intersection (x,y) pixel = (pixel == Scaffold) && isScaffold (x + 1, y) && isScaffold (x - 1, y) && isScaffold (x, y + 1) && isScaffold (x, y - 1)
        isScaffold pos = maybe False ((==) Scaffold) $ M.lookup pos vmap
        distances pos = (distance 0 pos (-1, 0), distance 0 pos (0, -1))
        distance units (x, y) dir@(x', y') = if M.member (x + x', y + y') vmap then distance (units + 1) (x + x', y + y') dir else units

buildMap :: [Int] -> PixelMap
buildMap = fst . foldr build (M.empty, (0, 0))
    where
        build pixel (vmap, pos@(x,y)) = case pixel of
            35 -> (M.insert pos Scaffold vmap, (x + 1, y))
            46 -> (M.insert pos Space vmap, (x + 1, y))
            60 -> (M.insert pos (Robot West) vmap, (x + 1, y))
            62 -> (M.insert pos (Robot East) vmap, (x + 1, y))
            94 -> (M.insert pos (Robot North) vmap, (x + 1, y))
            118 -> (M.insert pos (Robot South) vmap, (x + 1, y))
            88 -> (M.insert pos (Robot Tumbling) vmap, (x + 1, y))
            10 -> (vmap, (0, y + 1))

showMap :: [Int] -> String
showMap = map toEnum

{-
https://adventofcode.com/2019/day/17

An early warning system detects an incoming solar flare and automatically activates the ship's electromagnetic shield. Unfortunately, this has cut off the Wi-Fi for many small robots that, unaware of the impending danger, are now trapped on exterior scaffolding on the unsafe side of the shield. To rescue them, you'll have to act quickly!

The only tools at your disposal are some wired cameras and a small vacuum robot currently asleep at its charging station. The video quality is poor, but the vacuum robot has a needlessly bright LED that makes it easy to spot no matter where it is.

An Intcode program, the Aft Scaffolding Control and Information Interface (ASCII, your puzzle input), provides access to the cameras and the vacuum robot. Currently, because the vacuum robot is asleep, you can only access the cameras.

Running the ASCII program on your Intcode computer will provide the current view of the scaffolds. This is output, purely coincidentally, as ASCII code: 35 means #, 46 means ., 10 starts a new line of output below the current one, and so on. (Within a line, characters are drawn left-to-right.)

In the camera output, # represents a scaffold and . represents open space. The vacuum robot is visible as ^, v, <, or > depending on whether it is facing up, down, left, or right respectively. When drawn like this, the vacuum robot is always on a scaffold; if the vacuum robot ever walks off of a scaffold and begins tumbling through space uncontrollably, it will instead be visible as X.

In general, the scaffold forms a path, but it sometimes loops back onto itself. For example, suppose you can see the following view from the cameras:

..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^..
Here, the vacuum robot, ^ is facing up and sitting at one end of the scaffold near the bottom-right of the image. The scaffold continues up, loops across itself several times, and ends at the top-left of the image.

The first step is to calibrate the cameras by getting the alignment parameters of some well-defined points. Locate all scaffold intersections; for each, its alignment parameter is the distance between its left edge and the left edge of the view multiplied by the distance between its top edge and the top edge of the view. Here, the intersections from the above image are marked O:

..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^..
For these intersections:

The top-left intersection is 2 units from the left of the image and 2 units from the top of the image, so its alignment parameter is 2 * 2 = 4.
The bottom-left intersection is 2 units from the left and 4 units from the top, so its alignment parameter is 2 * 4 = 8.
The bottom-middle intersection is 6 from the left and 4 from the top, so its alignment parameter is 24.
The bottom-right intersection's alignment parameter is 40.
To calibrate the cameras, you need the sum of the alignment parameters. In the above example, this is 76.

Run your ASCII program. What is the sum of the alignment parameters for the scaffold intersections?
-}