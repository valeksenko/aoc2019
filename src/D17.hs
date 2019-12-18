module D17 (
    buildMap
  , showMap
  , PixelMap
  , Pixel(..)
  , Direction(..)
) where

import IntcodeV3
import Data.List
import qualified Data.Map as M

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

buildMap :: [Register] -> PixelMap
buildMap = fst . foldr build (M.empty, (0, 0)) . exec []
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
