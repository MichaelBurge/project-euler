import Data.List
import Data.Maybe
import Data.Ratio
import Random
import System.IO.Unsafe

g = unsafePerformIO $ getStdGen

isOkay (x,y) = x+y == 1000
tuples [] = []
tuples (x:xs) = [(x,y) | y <- xs] `union` (tuples xs)
lists n = snd $ mapAccumL (\x y -> (drop n x, take n $ map (`mod` 1000) x)) ((randoms g)::[Integer]) [1..]
listOkay = (any isOkay) . tuples
first xs = fromJust $ findIndex id $ map listOkay $ inits xs
firsts xs = i:(firsts $ drop i xs) where i = first xs
f i cs (x:xs) = 
  if any (== (1000 - x)) cs
    then i : (f 0 [] xs)
    else f (i+1) (x:cs) xs

average :: [Integer] -> Double
average xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)
sum' = foldl' (+) 0
answer n = sum' $ take n $ map (\x -> x % n) $ f 0 [] $ map (`mod` 1000) $ ((randoms g)::[Integer])

main = putStrLn $ show $ answer 10000000