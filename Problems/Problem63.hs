module Problems.Problem63 where

import Common.Digits
import Data.List
import Types

answer =
    length $
    nub $
    map (\(b, n) -> b^n) $
    filter (\(b, n) -> numDigits 10 (b^n) == n) $
    [(b, n) | b <- [1..9], n <- [0..60]]

problem63 = Problem {
    description = "Problem 63: Powerful Digit Counts",
    solution = show answer
}