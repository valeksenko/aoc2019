module D16 (
    calcFFT
) where

import Data.List

basePattern = [0, 1, 0, -1]

calcFFT :: [Integer] -> [Integer]
calcFFT input = map takeDigit . map calc $ [1..length input]
    where
        calc n = sum . map (uncurry (*)) $ zip input (base n)
        base n = tail . concatMap (replicate n) $ cycle basePattern
        takeDigit n = (abs n) `mod` 10