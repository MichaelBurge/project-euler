module Common.Sequence where
import Data.List

diffList ys@(x:xs) = zipWith (-) xs ys
sumList [] = []
sumList xs = snd $ mapAccumL (\a b -> (a+b,a+b)) 0 xs

triangles :: [Integer]
triangles = map (\n -> n * (n+1) `div` 2) [1..]

pentagonals :: [Integer]
pentagonals = map (\n -> (n * (3*n-1)) `div` 2) [1..]

hexagonals :: [Integer]
hexagonals = map (\n -> n * (2 * n -1)) [1..]

subseqs :: Int -> [Integer] -> [[Integer]]
subseqs n xs | length xs < n = []
subseqs n ys@(x:xs) = (take n ys) : (subseqs n xs)

subsequenceOf :: Eq a => [a] -> [a] -> Bool
subsequenceOf [] _  = True
subsequenceOf _ [] = False
subsequenceOf (a:as) (x:xs) =
    (subsequenceOf (a:as) xs) ||
    (x == a && subsequenceOf as xs)
