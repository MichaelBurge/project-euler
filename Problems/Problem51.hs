module Problems.Problem51 where

import Common.Digits (replaceDigits, numDigits)
import Common.Prime (prime, primes)
import Control.Monad (replicateM)
import Types

maxTry = 1000000

smallPrimes = takeWhile (<maxTry) $ primes

solutionPatternsUsing n = filter (isSolution n) $
                   replicateM (numDigits 10 n) [True, False]

primesWithPattern n pattern =
    filter prime $
    map (\d -> replaceDigits 10 d n pattern) [0..9]

isSolution n pattern =
    (==) 8 $
    length $
    filter (> (maxTry `div` 10)) $
    primesWithPattern n pattern

smallestSolution =
    let answers = 
            map (\(a, bss) -> (a, head bss)) $
            filter (\(a, bs) -> not $ null bs) $
            map (\n -> (n, solutionPatternsUsing n)) $
            smallPrimes
    in if null answers then (0,[]) else head answers

problem51 :: Problem
problem51 = Problem {
    description = "Problem 51: Prime Digit Replacements",
    solution = show smallestSolution
}