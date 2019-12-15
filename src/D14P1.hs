module D14P1 (
    parseReactions
  , orecount
) where

import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Extra((***))
import Data.List.Split(chunksOf)
import qualified Data.Map as M

type Material = String
type Ingridient = (Material, Double)
type Reaction = M.Map Material (Double, [Ingridient])

ore = "ORE"
fuel = "FUEL"

orecount :: Reaction -> (Double, [Double])
orecount reactions = oreCount 1 fuel (0, [])
    where
        oreCount required material actual = calcCost required actual $ findCost material
        calcCost required actual (amount, materials) = addCost (required / amount) . unzip $ map (materialCost actual) materials
        addCost n = ((*) n . sum) *** concat
        findCost material = fromJust $ M.lookup material reactions
        materialCost actual@(aTotal, aAmounts) (material, amount) = if (material == ore) then (aTotal + amount, amount:aAmounts) else oreCount amount material actual

parseReactions :: String -> Reaction
parseReactions = foldr (parse . words) M.empty . lines
    where
        parse w reactions = parseWords reactions $ break ((==) "=>") w
        parseWords reactions (src, res) = M.insert (fst . parseIngridient $ tail res) (snd . parseIngridient $ tail res, map parseIngridient $ chunksOf 2 src) reactions
        parseIngridient (amount:material:_) = (delete ',' material, (read amount) :: Double) 

{-
https://adventofcode.com/2019/day/14

As you approach the rings of Saturn, your ship's low fuel indicator turns on. There isn't any fuel here, but the rings have plenty of raw material. Perhaps your ship's Inter-Stellar Refinery Union brand nanofactory can turn these raw materials into fuel.

You ask the nanofactory to produce a list of the reactions it can perform that are relevant to this process (your puzzle input). Every reaction turns some quantities of specific input chemicals into some quantity of an output chemical. Almost every chemical is produced by exactly one reaction; the only exception, ORE, is the raw material input to the entire process and is not produced by a reaction.

You just need to know how much ORE you'll need to collect before you can produce one unit of FUEL.

Each reaction gives specific quantities for its inputs and output; reactions cannot be partially run, so only whole integer multiples of these quantities can be used. (It's okay to have leftover chemicals when you're done, though.) For example, the reaction 1 A, 2 B, 3 C => 2 D means that exactly 2 units of chemical D can be produced by consuming exactly 1 A, 2 B and 3 C. You can run the full reaction as many times as necessary; for example, you could produce 10 D by consuming 5 A, 10 B, and 15 C.

Suppose your nanofactory produces the following list of reactions:

10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
The first two reactions use only ORE as inputs; they indicate that you can produce as much of chemical A as you want (in increments of 10 units, each 10 costing 10 ORE) and as much of chemical B as you want (each costing 1 ORE). To produce 1 FUEL, a total of 31 ORE is required: 1 ORE to produce 1 B, then 30 more ORE to produce the 7 + 7 + 7 + 7 = 28 A (with 2 extra A wasted) required in the reactions to convert the B into C, C into D, D into E, and finally E into FUEL. (30 A is produced because its reaction requires that it is created in increments of 10.)

Or, suppose you have the following list of reactions:

9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
The above list of reactions requires 165 ORE to produce 1 FUEL:

Consume 45 ORE to produce 10 A.
Consume 64 ORE to produce 24 B.
Consume 56 ORE to produce 40 C.
Consume 6 A, 8 B to produce 2 AB.
Consume 15 B, 21 C to produce 3 BC.
Consume 16 C, 4 A to produce 4 CA.
Consume 2 AB, 3 BC, 4 CA to produce 1 FUEL.

Given the list of reactions in your puzzle input, what is the minimum amount of ORE required to produce exactly 1 FUEL?
-}