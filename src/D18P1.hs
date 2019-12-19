module D18P1 (
    stepcount
  , parseMap
  , Explorer(..)
  , Item(..)
) where

import IntcodeV3
import Data.List
import Data.Maybe
import Data.Char
import Data.Tuple.Extra
import qualified Data.Map as M
import Algorithm.Search (aStar)
import Debug.Trace

type Coordinate = (Int, Int)
type VaultMap = M.Map Coordinate Item

data Explorer =
    Explorer {
        ePosition :: Coordinate
      , eMap :: VaultMap
      , eKeys :: [Char]
    } deriving(Show, Eq, Ord)

data Item
    = Key Char
    | Door Char
    | Passage
    deriving(Show, Eq, Ord)

stepcount :: Explorer -> Int
stepcount = fst . fromJust . steps
    where
        steps = aStar neighbors cost remainingKeys keysCollected
        keysCollected = not . any isKey . M.elems . eMap
        remainingKeys = length . filter isKey . M.elems . eMap
        isKey (Key _) = True
        isKey _ = False
        cost _ _ = 1
        neighbors explorer = catMaybes $ map (neighbor explorer) [(0, 1), (0, -1), (1, 0), (-1, 0)]

neighbor :: Explorer -> Coordinate -> Maybe Explorer
neighbor explorer pos = (M.lookup newPos $ eMap explorer) >>= updatedExplorer
    where
        newPos = both sum $ unzip [pos, (ePosition explorer)]
        unlockedDoor c = elem (toLower c) $ eKeys explorer
        updatedExplorer item = case item of
            Passage -> Just $ explorer { ePosition = newPos }
            (Key c) -> Just $ Explorer newPos (M.insert newPos Passage (eMap explorer)) (c:(eKeys explorer))
            (Door c) -> if unlockedDoor c then Just $ explorer { ePosition = newPos, eMap = M.insert newPos Passage (eMap explorer) } else Nothing

parseMap :: String -> Explorer
parseMap = fst . foldl' build (Explorer (0, 0) M.empty [], (0,0))
    where
        item c = if (isLower c) then Key c else Door c
        build (explorer, (y, x)) c = case c of
            '.' -> (explorer { eMap = M.insert (x, y) Passage (eMap explorer) }, (y, x + 1))
            '#' -> (explorer, (y, x + 1))
            '@' -> (explorer { ePosition = (x, y), eMap = M.insert (x, y) Passage (eMap explorer) }, (y, x + 1))
            '\n' -> (explorer, (y + 1, 0))
            otherwise -> (explorer { eMap = M.insert (x, y) (item c) (eMap explorer) }, (y, x + 1))


{-
https://adventofcode.com/2019/day/18

A scan of the local area reveals only one interesting feature: a massive underground vault. You generate a map of the tunnels (your puzzle input). The tunnels are too narrow to move diagonally.

Only one entrance (marked @) is present among the open passages (marked .) and stone walls (#), but you also detect an assortment of keys (shown as lowercase letters) and doors (shown as uppercase letters). Keys of a given letter open the door of the same letter: a opens A, b opens B, and so on. You aren't sure which key you need to disable the tractor beam, so you'll need to collect all of them.

For example, suppose you have the following map:

#########
#b.A.@.a#
#########
Starting from the entrance (@), you can only access a large door (A) and a key (a). Moving toward the door doesn't help you, but you can move 2 steps to collect the key, unlocking A in the process:

#########
#b.....@#
#########
Then, you can move 6 steps to collect the only other key, b:

#########
#@......#
#########
So, collecting every key took a total of 8 steps.

Here is a larger example:

########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
The only reasonable move is to take key a and unlock door A:

########################
#f.D.E.e.C.b.....@.B.c.#
######################.#
#d.....................#
########################
Then, do the same with key b:

########################
#f.D.E.e.C.@.........c.#
######################.#
#d.....................#
########################
...and the same with key c:

########################
#f.D.E.e.............@.#
######################.#
#d.....................#
########################
Now, you have a choice between keys d and e. While key e is closer, collecting it now would be slower in the long run than collecting key d first, so that's the best choice:

########################
#f...E.e...............#
######################.#
#@.....................#
########################
Finally, collect key e to unlock door E, then collect key f, taking a grand total of 86 steps.

Here are a few more examples:

########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
Shortest path is 132 steps: b, a, c, d, f, e, g

#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
Shortest paths are 136 steps;
one is: a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m

########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
Shortest paths are 81 steps; one is: a, c, f, i, d, g, b, e, h

How many steps is the shortest path that collects all of the keys?
-}