module Common.Prime (modExp, isSprp, primes, prime, primeByPrp, primeByFactoring, primePower, factors) where
import Prelude hiding (filter,
                       map,
                       null
                      )
import qualified Prelude
import qualified Data.List as List
import qualified Data.Maybe
import Data.Set

sum' = fold (+) 0
primes = allPrimes

prime :: Integer -> Bool
prime = primeByPrp

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ _ 0 = 1
modExp mod base 1 = base `rem` mod
modExp mod base exponent = flip rem mod $
    let next = modExp mod (base * base `rem` mod) (exponent `div` 2)
    in next * (if odd exponent then base else 1)

primePower n p =
  let f x i = let (q, r) = x `quotRem` p
              in if r == 0
                 then f q (i+1)
                 else i
  in f n 0

oddPart n = if odd n then n else oddPart (n `div` 2)

-- See Section 2.3 of http://primes.utm.edu/prove/merged.html
isPrp n a = 1 == (modExp n a (n-1))
isSprp n a =
    let s  = primePower (n-1) 2
        d = oddPart $ n-1
    in (1 == modExp n a d) ||
       any (\r -> (n-1) == modExp n (a^d) (2^r)) [0..s]

-- See the table in Section 2.3 of http://primes.utm.edu/prove/merged.html
primeByPrp n = let
  checkPrp a = isSprp n a
  in case () of
       _ | n < 4 -> n == 2 || n == 3
         | smallPrimeDivides n -> False
         | n < 1373653 -> checkPrp 2 && checkPrp 3
         | n < 25326001 -> checkPrp 2 && checkPrp 3 && checkPrp 5
         | n < 118670087467 -> checkPrp 2 && checkPrp 3 && checkPrp 5 && checkPrp 7 && n /= 3215031751 
         | n < 2152302898747 -> all checkPrp [2,3,5,7,11]
         | n < 3474749660383 -> all checkPrp [2,3,5,7,11,13]
         | n < 341550071728321 -> all checkPrp [2,3,5,7,11,13,17]
         | otherwise -> error $ "Add more prp checks to primeByPrp. Received: " ++ show n

isFactor n k = 0 == (rem n k)
possibleFactors n = List.takeWhile (\x -> x*x <= n) [1..]
smallFactors n = fromList $ List.filter (isFactor n) (possibleFactors n)
largesFromSmalls n smalls = map (n `quot`) smalls
largeFactors n = largesFromSmalls n $ smallFactors n
smallPrimes = takeWhile (<200) primes
factors n = 
  let sf = smallFactors n
  in sf `union` (largesFromSmalls n sf)

smallPrimeDivides n = any (\p -> n /= p && n `rem` p == 0) smallPrimes

primeByFactoring n | n < 2 = False
primeByFactoring n | smallPrimeDivides n = False
primeByFactoring n = null $ (factors n) \\ (fromList [1, n])

allPrimes = (:) 2 $ 
         Prelude.filter (\x -> all (\y -> (x `rem` y) /= 0) $ 
                               (List.takeWhile (\y -> y*y <= x) allPrimes)
                        ) [3..]
-- Sundaram Sieve
-- DON'T USE - VERY SLOW ATM
upTo :: Integer -> [Integer]
upTo n = let markeds = fromList $ [i + j + 2 * i * j | i <- [1..n], j <- [i..n]]
         in 2 : (Data.Maybe.catMaybes $
                 List.map (\k ->
                               if k `member` markeds
                               then Nothing
                               else Just $ 2 * k + 1)
                 [1..n]
                )

primeFactors n = filter prime $ factors n
properFactors n = filter (<n) $ factors n
isAmicable n =
  let f = sum' . properFactors
  in (n == (f . f) n) && (n /= f n)
isAbundant n = (sum' $ properFactors n) > n 
abundants n = filter isAbundant $ fromList [1..n]
sumsFrom _ [] = empty
sumsFrom limit z@(x:xs) = union first (sumsFrom limit xs) 
  where first = fromList $ [x + y | y <- z, x+y < limit]
