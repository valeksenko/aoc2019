module D06 (
    buildorbital
  , Orbital(..)
) where

import Data.Digits
import Data.List
import Data.Tuple.Extra

type Orbit = (String, String)
data Orbital a = Body a [Orbital a] deriving (Eq, Ord, Show)

buildorbital :: [String] -> Orbital String
buildorbital = build . map parse
    where
        parse = second tail . break ((==) ')')

build :: [Orbit] -> Orbital String
build orbits = addOrbital $ findCOM orbits
    where
        addOrbital id = Body id $ addBodies id
        addBodies id = map (addOrbital . snd) $ filter ((==) id . fst) orbits

findCOM :: [Orbit] -> String
findCOM orbits = head $ (nub $ map fst orbits) \\ (nub $ map snd orbits)

-- buildTree :: [Int] -> [Node]
-- buildTree = fst . addNode 1

-- addNode :: Int -> [Int] -> ([Node], [Int])
-- addNode 0 l = ([], l)
-- addNode cnt (c:d:xs) = addN cnt c d (addNode c xs)
--     where
--         addN cnt c d (children, xs) = nodes (Node c d children (take d xs)) (addNode (cnt - 1) $ drop d xs)
--         nodes n (siblings, xs) = (n:siblings, xs)