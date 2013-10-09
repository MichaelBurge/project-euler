module Common.Prime
    (
     modExp,
     isSprp,
     primes,
     prime,
     primeByPrp,
     primeByFactoring,
     primePower,
     factors,
     rabin_miller
    ) where
import Prelude hiding (filter,
                       map,
                       null
                      )
import qualified Prelude
import qualified Data.List as List
import qualified Data.Maybe
import Data.Set
import System.Random

sum' = fold (+) 0
primes = allPrimes

prime :: Integer -> Bool
prime = primeByPrp

maybePrime :: StdGen -> Integer -> Bool
maybePrime = rabin_miller

modExp :: (Num a, Integral a, Show a) => a -> a -> a -> a
modExp mod base n | mod < 0 = error ("Bad modExp modulus: " ++ show mod)
modExp mod base n | n < 0 = error ("Bad modExp exponent: " ++ show n)
modExp modulus base n | base < 0 = modExp modulus (n - (base `mod` modulus)) n
modExp _ _ 0 = 1
modExp mod base 1 = base `rem` mod
modExp mod base exponent = flip rem mod $
    let next = modExp mod (base * base `rem` mod) (exponent `div` 2)
    in next * (if odd exponent then base else 1)

-- | Write n = p^k * q. Returns (q, k).
primeFreeRem n p = 
 let f x i = let (q, r) = x `quotRem` p
             in if r == 0
                 then f q (i+1)
                 else (x, i)
  in f n 0

primeFree n p = fst $ primeFreeRem n p
primePower n p = snd $ primeFreeRem n p 


oddPart n = if odd n then n else oddPart (n `div` 2)

-- See Section 2.3 of http://primes.utm.edu/prove/merged.html
isPrp n a = 1 == (modExp n a (n-1))
isSprp :: (Eq a, Num a, Integral a, Show a) => a -> a -> Bool
isSprp n a =
    let s  = primePower (n-1) 2
        d = oddPart $ n-1
        a' = modExp n a d
    in (1 == a') ||
       any (\r -> (n-1) == modExp n a' (2^r)) [0..s]

-- See the table in Section 2.3 of http://primes.utm.edu/prove/merged.html
primeByPrp :: Integer -> Bool
primeByPrp n = let
  checkPrpFast a = isSprp (fromIntegral n :: Int) (fromIntegral a)
  checkPrpSlow a = isSprp n a
  checkPrp = if n < (fromIntegral (maxBound::Int))
               then checkPrpFast
               else checkPrpSlow
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
smallPrimes = takeWhile (<250) primes
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

rabin_miller :: StdGen -> Integer -> Bool
rabin_miller _ n | n <= 3 = prime n
rabin_miller _ n | even n = False
rabin_miller stdGen n =
    let (q, t) = primeFreeRem (n-1) 2 
        maxBound = 20
        checkSquarings b = 
            let cond n _ | n < 0 = error $ "Invalid n: " ++ show n
                cond 0 _ = False
                cond i x = 
                    let newX = modExp n x 2
                    in case () of
                       _ | newX == 1     -> False
                         | newX == n - 1 -> True
                         | otherwise     -> cond (i-1) newX
            in cond (t-1) b
        checkBase a =
            let b = modExp n a q
            in if (b == 1) || (b == (n - 1))
               then True
               else checkSquarings b
    in let xs = take maxBound $
                List.filter (\x -> x /= 0 && x /= 1 && x /= (n-1)) $
                List.map (\x -> x `mod` n) $
                randoms stdGen
       in all id $
          List.map checkBase $
          xs