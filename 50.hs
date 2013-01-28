import Common.Prime
import Data.List
smallPrimes = takeWhile (<1000) primes
biggers = filter (\xs -> (length xs) > 21)
subseqs = biggers $ concat $ map inits $ biggers $ tails $ smallPrimes

-- Can't run along primes splitting at non-primes - even if a sum is non-prime, you could still add a prime to it to get a prime
-- Certainly if a prime sum consumes another(is a superset of the other), we can forget about the consumed. So maybe we still need to only look at non-prime runs?
count :: Integer -> Int
count p = length $ takeWhile prime $ scanl (+) p $ filter (>p) primes

main = putStrLn $ show $ length $ subseqs