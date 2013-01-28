import Common.Digits
import Control.Parallel.Strategies

prod :: Integer -> [Integer] -> Integer
prod x ys = 
    fromDigits 10 $
    reverse $
    foldl (++) [] $
    map (reverse . toDigits 10) $
    map (x*) ys

numDigits 0 = []
numDigits n = [10^(n-1) .. 10^n - 1]

f a b = do
  x <- numDigits a
  let r = prod x [1..b]
  if isPandigital 10 r
  then return r
  else []

g = maximum $ parMap rpar (\(a,b) -> f a b) [(1,9), (2,4), (3,3), (4,2)]

main = putStrLn $ show g