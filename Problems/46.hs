import Common.Prime
import Control.Parallel.Strategies

buf = 10000
sps = take buf primes
sqs = takeWhile (\x -> x < (fromIntegral buf)) $ map (\x -> x * x) [1..]

isGood x = 
    (x /= 1) &&
    (odd x) &&
    (not $ prime x) &&
    (not $ any (\(p,s) -> x == p + 2 * s) [(p,s) | p <- sps, s <- sqs])

answer = take 3 $ filter isGood [1..fromIntegral buf]

main = putStrLn $ show answer