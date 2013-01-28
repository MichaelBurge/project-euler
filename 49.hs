import Common.Digits
import Common.Prime
import Data.List
import qualified Data.Set as S
dedup = nubBy (\x y -> elem x $ map (fromDigits 10) $ permutations $ toDigits 10 y)

l1 = filter (>1000) $ takeWhile (<10000) primes
l2 = dedup l1
choices = S.fromList $ do
  b <- l2
  let ps = filter prime $ map (fromDigits 10) $ permutations $ toDigits 10 b
  a <- ps
  b <- filter (/= a) ps
  c <- filter (\x -> x /= a && x /= b) ps
  if ((max a b) - (min a b) == (max b c) - (min b c))
     then return [a,b,c]
     else []


main = putStrLn $ show choices