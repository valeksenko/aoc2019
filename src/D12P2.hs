module D12P2 (
    stepcount
  , show3d
) where

import D12
import Data.List
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.IO.Simulate

show3d :: [Coordinate] -> IO ()
show3d c = simulateIO window black 1 (mkSimullation c) viewSimullation stepSimullation

window :: Display
window = InWindow "Moon movements" (1400, 1400) (0, 0)

project :: Coordinate -> Point
project (x0, y0, z0) = (projectedX, projectedY)
    where (lookAtX, lookAtY, lookAtZ) = (0.0, 0.0, 0.0)
          (x, y, z) = ((fromIntegral x0) - lookAtX, (fromIntegral y0) - lookAtY, (fromIntegral z0) - lookAtZ)
          (alpha, beta, gamma) = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
          (eyeX, eyeY, eyeZ) = (0.0, 0.0, 300.0)
          (cosAlpha, sinAlpha) = (cos alpha, sin alpha)
          (cosBeta, sinBeta) = (cos beta, sin beta)
          (cosGamma, sinGamma) = (cos gamma, sin gamma)
          (dx, dy, dz) = (cosBeta*(sinGamma*y + cosGamma*x) - sinBeta*z,
                          sinAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) +
                          cosAlpha*(cosGamma*y - sinGamma*x),
                          cosAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) -
                          sinAlpha*(cosGamma*y - sinGamma*x)) 
          projectedX = eyeZ/dz*dx - eyeX
          projectedY = eyeZ/dz*dy - eyeY


viewSimullation :: [Body] -> IO Picture
viewSimullation moons = return $ pictures [Color blue $ showMoon newPos]
    where
        showMoon p@(x,y) = translate x y $ thickCircle 0 20
        newPos = project . snd $ head moons

stepSimullation :: ViewPort -> Float -> [Body] -> IO [Body]
stepSimullation _ _ moons = return $ runSimullation moons


stepcount :: [Coordinate] -> Int
stepcount = const 1
    where
        run sim = until ((==) sim . snd) runStep (1, runSimullation sim)
        runStep (c, sim) = (traceShow . snd $ head sim) $ (c + 1, runSimullation sim)

{-
https://adventofcode.com/2019/day/12#part2

All this drifting around in space makes you wonder about the nature of the universe. Does history really repeat itself? You're curious whether the moons will ever return to a previous state.

Determine the number of steps that must occur before all of the moons' positions and velocities exactly match a previous point in time.

For example, the first example above takes 2772 steps before they exactly match a previous point in time; it eventually returns to the initial state:

After 0 steps:
pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>

After 2770 steps:
pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>

After 2771 steps:
pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>

After 2772 steps:
pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
Of course, the universe might last for a very long time before repeating. Here's a copy of the second example from above:

<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>
This set of initial positions takes 4686774924 steps before it repeats a previous state! Clearly, you might need to find a more efficient way to simulate the universe.

How many steps does it take to reach the first state that exactly matches a previous state?
-}