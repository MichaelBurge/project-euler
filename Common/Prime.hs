module Common.Prime where
import Prelude hiding (filter,
                       map,
                       null
                      )
import qualified Prelude
import qualified Data.List as List
import qualified Data.Maybe
import Data.Set

sum' = fold (+) 0

--primes = upTo 10000
primes = allPrimes
--primes = takeWhile (<238208) allPrimes

isFactor n k = 0 == (rem n k)
possibleFactors n = fromList $ List.takeWhile (\x -> x*x <= n) [1..]
smallFactors n = filter (isFactor n) (possibleFactors n)
largesFromSmalls n smalls = map (n `quot`) smalls
largeFactors n = largesFromSmalls n $ smallFactors n
smallPrimes = takeWhile (<200) primes
factors n = 
  let sf = smallFactors n
  in sf `union` (largesFromSmalls n sf)

smallPrimeDivides n = any (\p -> n /= p && n `rem` p == 0) smallPrimes

prime n | n < 2 = False
prime n | smallPrimeDivides n = True
prime n = null $ (factors n) \\ (fromList [1, n])

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
