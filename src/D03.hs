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
