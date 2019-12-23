module D22 (
  shuffled
  , ShuffleTechnique(..)
  , Card
) where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple

type Card = Int
type Cards = [Card]

data ShuffleTechnique
    = DealIntoNewStack
    | Cut Int
    | DealWithIncrement Int
    deriving(Show, Eq, Ord)

shuffled :: Int -> Int -> [ShuffleTechnique] -> Cards
shuffled amount times techniques = last . take (times + 1) $ iterate shuffleDeck [0..(amount - 1)] 
    where
        shuffleDeck cards = foldl' shuffle cards techniques


shuffle :: Cards -> ShuffleTechnique -> Cards

shuffle cards (DealIntoNewStack) = reverse cards

shuffle cards (Cut n) = if n < 0 then cut (length cards + n) else cut n
    where
        cut i = uncurry (flip (++)) $ splitAt i cards

shuffle cards (DealWithIncrement n) = fst . unzip . sortBy (comparing snd) $ zip cards [ i `mod` (length cards) | i <- [0,n..] ]
