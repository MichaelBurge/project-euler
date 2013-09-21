module Problems.Problem79 where

import Common.Digits
import Common.Sequence
import Data.Char
import Data.Functor
import Data.List
import System.IO.Unsafe
import Types

patterns = nub $ sort $ unsafePerformIO $
    map (map digitToInt) <$>
    lines <$>
    readFile "Problems/Problem79.txt"

allPatternsMatch :: [Int] -> Bool
allPatternsMatch xs = all (\ys -> ys `subsequenceOf` xs) patterns

answer =
    stripUnneeded allPatternsMatch $ concat patterns

problem79 = Problem {
    description = "Problem 79: Passcode Derivation",
    solution = show $ fromDigits 10 $ map fromIntegral answer
}