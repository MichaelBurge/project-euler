import Common.Digits
import Data.Char
import Data.List

op x = case x of
         89 -> 89
         1 -> 1
         otherwise -> sum $ map (^2) $ toDigits 10 x
fixed x =
  let x' = op x
  in if x' == x
      then x
      else fixed x'
main = putStrLn $ show $ length $ filter (==89) $ map fixed [1..10000000]