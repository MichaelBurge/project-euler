import Data.List

isFactor k n = 0 == (rem n k)
possibleFactors n = takeWhile (\x -> x*x <= n) [1..]
smallFactors n = filter (\x -> isFactor x n) (possibleFactors n)
largeFactors n = map (\x -> n `quot` x) $ smallFactors n
factors n = (smallFactors n) ++ (largeFactors n)
prime n = ((factors n) \\ [1, n]) == []
primeFactors n = filter prime (factors n)