import Common.Prime
import Common.Sequence
import Data.List

_size = 1000000
_min = 500
_length = 600

smallPrimes n = takeWhile (<n) primes
subseqprimes k n = filter prime $
               takeWhile (<n) $
               map (foldl1 (+)) $
               subseqs k $
               takeWhile (<n) primes

up n k = let
    seq = subseqprimes k n
    in if null seq
       then (k, -1)
       else (k, head seq)

upto n k = mapM_ (putStrLn . show) $ map (up n) [_min..k]

-- Can't run along primes splitting at non-primes - even if a sum is non-prime, you could still add a prime to it to get a prime
-- Certainly if a prime sum consumes another(is a superset of the other), we can forget about the consumed. So maybe we still need to only look at non-prime runs?
count p = length $ filter prime $ scanl (+) p $ filter (>p) primes

main = upto _size (fromIntegral _length)