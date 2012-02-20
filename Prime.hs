module Prime where
import Prelude hiding (filter,
                       map,
                       null
                      )
import qualified Data.List as List
import Data.Set

sum' = fold (+) 0

isFactor n k = 0 == (rem n k)
possibleFactors n = fromList $ List.takeWhile (\x -> x*x <= n) [1..]
smallFactors n = filter (isFactor n) (possibleFactors n)
largeFactors n = map (n `quot`) $ smallFactors n
factors n = (smallFactors n) `union` (largeFactors n)
prime n = if n < 2 
          then False
          else null $ (factors n) \\ (fromList [1, n])
primeFactors n = filter prime (factors n)
properFactors n = filter (<n) $ factors n
isAmicable n =
  let f = sum' . properFactors
  in (n == (f . f) n) && (n /= f n)
isAbundant n = (sum' $ properFactors n) > n 
abundants n = filter isAbundant $ fromList [1..n]
sumsFrom _ [] = empty
sumsFrom limit z@(x:xs) = union first (sumsFrom limit xs) 
  where first = fromList $ [x + y | y <- z, x+y < limit]
