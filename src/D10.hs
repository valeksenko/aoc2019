module D10 ( 
    parseSpaceMap
  , visibleAsteroids
  , allAsteroids
  , Coordinate
  , SpaceObject(..)
) where

import Data.List

type Coordinate = (Int, Int)

data SpaceObject
    = Empty
    | Asteroid
    deriving(Show, Eq)

visibleAsteroids :: [Coordinate] -> Coordinate -> [Coordinate]
visibleAsteroids asteroids c = filter (isVisible asteroids c) asteroids

allAsteroids :: [(SpaceObject, Coordinate)] -> [Coordinate]
allAsteroids = map snd . filter ((==) Asteroid . fst)

isVisible :: [Coordinate] -> Coordinate -> Coordinate -> Bool
isVisible coords c1@(y1, x1) c2@(y2, x2) = if c1 == c2 then False else not (any blocking coords)
    where
        blocking c@(x, y) = (c /= c1) && (c /= c2) && ((angle c) == lineOfSight) -- && (between x x1 x2) && (between y y1 y2)
        lineOfSight = angle c2
        angle (y,x) = atan ((fromIntegral $ y1 - y) / (fromIntegral $ x1 - x))
        between b a1 a2 = abs (a1 - a2) >= abs (a1 - b)

parseSpaceMap :: String -> [(SpaceObject, Coordinate)]
parseSpaceMap = fst . foldl' parse ([], (0, 0))
    where
        parse (space, (y, x)) c = case c of
            '.' -> (space ++ [(Empty, (y, x))], (y, x + 1))
            '#' -> (space ++ [(Asteroid, (y, x))], (y, x + 1))
            ' ' -> (space, (y, x))
            '\n' -> (space, (y + 1, 0))


{-
https://adventofcode.com/2019/day/10

The Elves would like to build a new monitoring station in a nearby area of space; they hand you a map of all of the asteroids in that region (your puzzle input).

The map indicates whether each position is empty (.) or contains an asteroid (#). The asteroids are much smaller than they appear on the map, and every asteroid is exactly in the center of its marked position. The asteroids can be described with X,Y coordinates where X is the distance from the left edge and Y is the distance from the top edge (so the top-left corner is 0,0 and the position immediately to its right is 1,0).

Your job is to figure out which asteroid would be the best place to build a new monitoring station. A monitoring station can detect any asteroid to which it has direct line of sight - that is, there cannot be another asteroid exactly between them. This line of sight can be at any angle, not just lines aligned to the grid or diagonally. The best location is the asteroid that can detect the largest number of other asteroids.

For example, consider the following map:

.#..#
.....
#####
....#
...##
The best location for a new monitoring station on this map is the highlighted asteroid at 3,4 because it can detect 8 asteroids, more than any other location. (The only asteroid it cannot detect is the one at 1,0; its view of this asteroid is blocked by the asteroid at 2,2.) All other asteroids are worse locations; they can detect 7 or fewer other asteroids. Here is the number of other asteroids a monitoring station on each asteroid could detect:

.7..7
.....
67775
....7
...87
Here is an asteroid (#) and some examples of the ways its line of sight might be blocked. If there were another asteroid at the location of a capital letter, the locations marked with the corresponding lowercase letter would be blocked and could not be detected:

#.........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c
Here are some larger examples:

Best is 5,8 with 33 other asteroids detected:

......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
Best is 1,2 with 35 other asteroids detected:

#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
Best is 6,3 with 41 other asteroids detected:

.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
Best is 11,13 with 210 other asteroids detected:

.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
Find the best location for a new monitoring station. How many other asteroids can be detected from that location?
-}