module Problems.Problem57 where

import Data.Function (on)
import Data.Ratio

import Common.Digits
import Types

iterates :: [Rational]
iterates = iterate (\x -> 2 + recip x) 2
matches :: Rational -> Bool
matches rat = on (>) (length . toDigits 10) (numerator rat) (denominator rat)
answer = length $ filter matches $ take 1000 $ map (\x -> 1 + recip x) iterates

problem57 = Problem {
    description = "Problem 57: Square root convergents",
    solution = show answer
}