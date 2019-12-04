module D04P1 (
    passwordcount
) where

import Data.Digits
import Data.List

passwordcount :: (Int, Int) -> Int
passwordcount (r1, r2) = length [i | i <- [r1..r2], validPassword (digits 10 i)]

validPassword :: [Int] -> Bool
validPassword ds = rightLength && hasDouble && increaseOrder
    where
        rightLength = length ds == 6
        hasDouble = any ((>1) . length) $ group ds
        increaseOrder = ds == sort ds

{-
https://adventofcode.com/2019/day/4

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

It is a six-digit number.
The value is within the range given in your puzzle input.
Two adjacent digits are the same (like 22 in 122345).
Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
Other than the range rule, the following are true:

111111 meets these criteria (double 11, never decreases).
223450 does not meet these criteria (decreasing pair of digits 50).
123789 does not meet these criteria (no double).
How many different passwords within the range given in your puzzle input meet these criteria?

Your puzzle input is 382345-843167.
-}