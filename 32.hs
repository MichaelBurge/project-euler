import Digits
import Data.List
import Data.Function
import qualified Data.Set as Set

isPandigital n a b =
  (on) (==) sort ((toDigits 10 (on (*) (fromDigits 10) a b))) (([1..n] \\ a) \\ b)
pandigitals n = do
  as <- filter (not . null) $ subsequences [1..n]
  a <- permutations as
  bs <- filter (not . null) $ subsequences ([1..n] \\ a)
  b <- permutations bs
  if isPandigital n a b
     then [(a,b)]
     else []
          
realSolution = nubBy (\(a,b) (c,d) -> a == d && b == c) $ pandigitals 9