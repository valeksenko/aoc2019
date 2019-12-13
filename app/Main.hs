module Main where

import D10
import D10P1

main :: IO ()
main = do
    -- print $ stepcount [
    --         (17,-9,4), (2,2,-13),(-1,5,-1),(4,7,-7)
    --     ]
    print $ maxasteroidcount [
                    (Empty,(0,0)),(Asteroid,(0,1)),(Empty,(0,2)),(Empty,(0,3)),(Asteroid,(0,4)),(Empty,(0,5)),(Empty,(0,6)),(Asteroid,(0,7)),(Asteroid,(0,8)),(Asteroid,(0,9)),(Asteroid,(1,0)),(Asteroid,(1,1)),(Asteroid,(1,2)),(Asteroid,(1,3)),(Empty,(1,4)),(Asteroid,(1,5)),(Asteroid,(1,6)),(Asteroid,(1,7)),(Empty,(1,8)),(Asteroid,(1,9)),(Empty,(2,0)),(Empty,(2,1)),(Empty,(2,2)),(Empty,(2,3)),(Asteroid,(2,4)),(Asteroid,(2,5)),(Asteroid,(2,6)),(Empty,(2,7)),(Asteroid,(2,8)),(Empty,(2,9)),(Empty,(3,0)),(Empty,(3,1)),(Asteroid,(3,2)),(Asteroid,(3,3)),(Asteroid,(3,4)),(Empty,(3,5)),(Asteroid,(3,6)),(Asteroid,(3,7)),(Empty,(3,8)),(Asteroid,(3,9)),(Asteroid,(4,0)),(Asteroid,(4,1)),(Empty,(4,2)),(Asteroid,(4,3)),(Asteroid,(4,4)),(Empty,(4,5)),(Asteroid,(4,6)),(Empty,(4,7)),(Asteroid,(4,8)),(Empty,(4,9)),(Empty,(5,0)),(Empty,(5,1)),(Empty,(5,2)),(Empty,(5,3)),(Asteroid,(5,4)),(Asteroid,(5,5)),(Asteroid,(5,6)),(Empty,(5,7)),(Empty,(5,8)),(Asteroid,(5,9)),(Empty,(6,0)),(Empty,(6,1)),(Asteroid,(6,2)),(Empty,(6,3)),(Asteroid,(6,4)),(Empty,(6,5)),(Empty,(6,6)),(Asteroid,(6,7)),(Empty,(6,8)),(Asteroid,(6,9)),(Asteroid,(7,0)),(Empty,(7,1)),(Empty,(7,2)),(Asteroid,(7,3)),(Empty,(7,4)),(Asteroid,(7,5)),(Empty,(7,6)),(Asteroid,(7,7)),(Asteroid,(7,8)),(Asteroid,(7,9)),(Empty,(8,0)),(Asteroid,(8,1)),(Asteroid,(8,2)),(Empty,(8,3)),(Empty,(8,4)),(Empty,(8,5)),(Asteroid,(8,6)),(Asteroid,(8,7)),(Empty,(8,8)),(Asteroid,(8,9)),(Empty,(9,0)),(Empty,(9,1)),(Empty,(9,2)),(Empty,(9,3)),(Empty,(9,4)),(Asteroid,(9,5)),(Empty,(9,6)),(Asteroid,(9,7)),(Empty,(9,8)),(Empty,(9,9))
                ]
