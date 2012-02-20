import Data.Function
import Data.List

collatz n | even n = n `div` 2
collatz n | odd n = 3 * n + 1

collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (collatz n)

longestSequence n = maximumBy (compare `on` snd) $ map (\x -> (x, length $ collatzSequence x)) [1..n]

main = putStrLn $ show $ longestSequence 1000000