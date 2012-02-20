module Digits where
import Data.Function
import Data.List
import Data.Ratio

toDigits b 0 = []
toDigits b n = (n `rem` b) : (toDigits b $ n `quot` b)

fromDigits :: Integer -> [Integer] -> Integer
fromDigits b = 
  let f i [] = []
      f i (x:xs) = (i * x) : (f (i*b) xs)
  in sum . (f 1)
     
reduced x y = (fromDigits 10 $ (toDigits 10 x) \\ (toDigits 10 y))

-- This needs work to be used for all fractions; only valid for n <= 100.
cancellingFractions n = [(a, b) | a <- [1..n], b <- [1..n], a < b, a * (reduced b a) == b * (reduced a b), a /= (reduced a b), b /= (reduced b a), a /= 10*(reduced a b), b /= 10*(reduced b a), (toDigits 10 a) /= (reverse $ toDigits 10 b), b /= 10*a, b /= 100*a]

isPalindrome b n = (\x -> x == (reverse x)) (toDigits b n)

combsTo _ [] = [0]
combsTo v (x:xs) = filter (==v) $ do
  a <- [0,x..v]
  b <- combsTo (v - a) xs
  return $ a + b
  
rotate ys = ((++) `on` ($ ys)) tail (return . head)
rotations xs = 
  let f ys = if next == xs
                then [ys]
                else ys:(f next)
             where next = rotate ys
   in f xs

nextPerm :: [Integer] -> Maybe [Integer]
nextPerm [] = Nothing
nextPerm (x:xs) = 
  case nextPerm xs of
    Nothing -> 
      let gs = List.filter (>x) xs
          a = minimum gs
      in if List.null gs
         then Nothing
         else Just $ a:(List.sort(x:(xs List.\\ [a])))
    Just ys -> Just $ x:ys

divDigits n x =
  let val = (10*n) `div` x 
  in (n,val) : (divDigits ((10*n) - x * val) x)
     
initialCycle n =
  takeWhile (\x -> (length x) == (length $ List.nub x)) $ List.inits $ divDigits 1 n


repeatingCycle n =
  let rep = last $ head $ dropWhile (\x -> (length x) == (length $ List.nub x)) $ List.inits $ divDigits 1 n
      f = (/=) rep
  in List.map snd $ (:) rep $ takeWhile f $ tail $ dropWhile f $ divDigits 1 n

biggestCycleTo n =
  maximum $ List.map List.length $ List.map repeatingCycle [1..n]