module D13P2 (
    gamescore
  , showGame
) where

import IntcodeV3
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.ViewPort

type Coordinate = (Int, Int)
type GameMap = M.Map Coordinate BlockTile

data BlockTile
    = Wall
    | Block
    | Paddle
    | Ball
    | Score Int
    deriving(Show, Eq)

data Game =
    Game {
        gState :: State
      , gBalls :: [Coordinate]
      , gPaddle :: Coordinate
      , gMap :: GameMap
    } deriving(Show, Eq)


thickness = 20.0

window :: Display
window = InWindow "Moon movements" (1300, 600) (0, 0)

gamescore :: [Register] -> Maybe BlockTile
gamescore = M.lookup (-1,0) . gMap . until gameOver playGame . mkGame
    where
        gameOver = M.null . M.filter ((==) Block) . gMap

showGame :: [Register] -> IO ()
showGame code = simulate window black 1 (mkGame code) gameState stepGame

gameState :: Game -> Picture
gameState = pictures . map pixel . M.toList . gMap
    where
        asPoint (x,y) = ((fromIntegral x) * thickness, - (fromIntegral y) * thickness)
        move = uncurry translate . asPoint
        pixel (c, e) = case e of
                Wall -> Color green $ move c $ rectangleSolid thickness thickness
                Block -> Color orange $ move c $ rectangleWire thickness thickness
                Paddle -> Color yellow $ move c $ rectangleUpperSolid thickness thickness
                Ball -> Color white $ move c $ circleSolid thickness
                Score n -> translate 100 20 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ show n

stepGame :: ViewPort -> Float -> Game -> Game
stepGame _ _ = playGame

playGame :: Game -> Game
playGame game = updateGame (makeMove game) game

mkGame :: [Register] -> Game
mkGame = newGame . runProgram . mkIntcode 'G' [] . (2:) . tail
    where
        newGame state = initBalls . updateGame state $ Game state [] (0,0) (mapOutput M.empty state)
        initBalls game = game { gBalls = (gBalls game) ++ (gBalls game) }

updateGame :: State -> Game -> Game
updateGame state = updatePositions . updateState
    where
        updateState game = game { gState = state, gMap = mapOutput (gMap game) state }
        updatePositions game = game { gBalls = (findTile Ball $ gMap game):(gBalls game), gPaddle = (findTile Paddle $ gMap game) }

makeMove :: Game -> State
makeMove game = runProgram $ (gState game) { sInput = [nextMove $ gBalls game], sOutput = [] }
    where
        nextMove (b1:b2:_) = newDirection (findTile Paddle $ gMap game) b1 $ nextPosition b1 b2
        nextPosition (x,y) (x',y') = case (nextTile (x + x - x') y) of
            Just Paddle -> x
            otherwise -> x + x - x'
        nextTile x y = M.lookup (x, y) (gMap game)
        newDirection (px,py) b nextb = if (px,py-1) == b then 0 else case (compare px nextb) of
                EQ -> 0
                LT -> 1
                GT -> -1

findTile :: BlockTile -> GameMap -> Coordinate
findTile t = fst . fromJust . find ((==) t . snd) . M.toList

mapOutput :: GameMap -> State -> GameMap
mapOutput gmap = foldl' mapOps gmap . (toOps []) . sOutput
    where
        toOps ops [] = ops
        toOps ops (tid:y:x:output) = toOps ((tid, y, x):ops) output
        mapOps omap (tid,y,x) = case tid of
            0 -> M.delete (x,y) omap
            1 -> M.insert (x,y) Wall omap
            2 -> M.insert (x,y) Block omap
            3 -> M.insert (x,y) Paddle omap
            4 -> M.insert (x,y) Ball omap
            otherwise -> M.insert (x,y) (Score tid) omap

{-
https://adventofcode.com/2019/day/13#part2

The game didn't run because you didn't put in any quarters. Unfortunately, you did not bring any quarters. Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.

The arcade cabinet has a joystick that can move left and right. The software reads the position of the joystick with input instructions:

If the joystick is in the neutral position, provide 0.
If the joystick is tilted to the left, provide -1.
If the joystick is tilted to the right, provide 1.
The arcade cabinet also has a segment display capable of showing a single number that represents the player's current score. When three output instructions specify X=-1, Y=0, the third output instruction is not a tile; the value instead specifies the new score to show in the segment display. For example, a sequence of output values like -1,0,12345 would show 12345 as the player's current score.

Beat the game by breaking all the blocks. What is your score after the last block is broken?
-}