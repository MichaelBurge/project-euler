import Common.Digits
import Common.Prime
import Data.List

drops k [] = []
drops k (x:xs) =
  let l = take k xs
  in if length l < 3
     then []
     else l : (drops k xs)

divCheck 2 (x:xs) = x `elem` [0,2,4,6,8]
divCheck 3 xs = (sum xs) `isFactor` 3
divCheck 5 (x:xs) = x `elem` [0,5]
{-
divCheck 11 xs = 
  let s p = sum $ filter p xs
      o = s odd
      e = s even
  in (o - e) `isFactor` 11
-}
divCheck n xs = (fromDigits 10 xs) `isFactor` n

ourPrimes = (takeWhile (<19) primes)

isSpecial :: [Integer] -> Bool
isSpecial xs = all (\(a,b) -> divCheck b a) $
               zip (map reverse $ drops 3 $ reverse xs) ourPrimes

f = do
  xs <- permutations [0..9]
  let good = isSpecial xs
  if good
     then return $ fromDigits 10 xs
     else []

answer = sum f

main = putStrLn $ show answer