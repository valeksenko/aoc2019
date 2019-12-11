module D11P2 (
    paintregistration
) where

import IntcodeV3
import D11
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List
import Data.Sequence((|>))
import Data.Foldable (toList)

paintregistration :: [Register] -> [String]
paintregistration = showPanelMap initCanvas . map fst . filter ((==) white . snd) . M.toList . paintHull white
    where 
        initCanvas = S.replicate 10 $ S.replicate 50 ' '
        showPanelMap canvas = map toList . toList . foldr draw canvas
        draw (x, y) canvas = S.update y (S.update x 'X' $ S.index canvas y) canvas

{-
https://adventofcode.com/2019/day/11#part2

You're not sure what it's trying to paint, but it's definitely not a registration identifier. The Space Police are getting impatient.

Checking your external ship cameras again, you notice a white panel marked "emergency hull painting robot starting panel". The rest of the panels are still black, but it looks like the robot was expecting to start on a white panel, not a black one.

Based on the Space Law Space Brochure that the Space Police attached to one of your windows, a valid registration identifier is always eight capital letters. After starting the robot on a single white panel instead, what registration identifier does it paint on your hull?
-}