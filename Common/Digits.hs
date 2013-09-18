module Common.Digits where
import Data.Char
import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Digits

numDigits :: Integer -> Integer -> Int
numDigits 10 = length . show
numDigits b = length . toDigits b

foldDigits :: Integer -> (a -> Int -> a) -> a -> Integer -> a
foldDigits _ _ acc 0 = acc
foldDigits 10 f acc n = foldr (flip f) acc $ map (fromIntegral . digitToInt) $ show n
foldDigits base f acc n = 
  let (q, r) = n `quotRem` base
      digit = fromIntegral r
  in foldDigits base f (f acc digit ) q


toDigits b 0 = []
toDigits 10 n = map (fromIntegral . digitToInt) $ show n
toDigits b n = Data.Digits.digits b n

fromDigits :: Integer -> [Integer] -> Integer
fromDigits = Data.Digits.unDigits

replaceDigits :: Integer -> Integer -> Integer -> [Bool] -> Integer
replaceDigits base value n positions =
    fromDigits base $
    zipWith (\d b -> if b then value else d) (toDigits base n) positions

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
      let gs = filter (>x) xs
          a = minimum gs
      in if null gs
         then Nothing
         else Just $ a:(sort(x:(xs \\ [a])))
    Just ys -> Just $ x:ys

divDigits n x =
  let val = (10*n) `div` x 
  in (n,val) : (divDigits ((10*n) - x * val) x)
     
initialCycle n =
  takeWhile (\x -> (length x) == (length $ nub x)) $ inits $ divDigits 1 n


repeatingCycle n =
  let rep = last $ head $ dropWhile (\x -> (length x) == (length $ nub x)) $ inits $ divDigits 1 n
      f = (/=) rep
  in map snd $ (:) rep $ takeWhile f $ tail $ dropWhile f $ divDigits 1 n

biggestCycleTo n =
  maximum $ map length $ map repeatingCycle [1..n]
  
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

pad n xs x | (length xs) >= n = xs
pad n xs x = pad n (xs ++ [x]) x

descendings :: Int -> Int -> [[Int]]
descendings b d = 
    let descendings' _ 0 = [[]]
        descendings' b d = do
          x <- [0..b]
          xs <- descendings' x $ d-1
          return $ x:xs
    in descendings' (b-1) d

-- See Wikipedia article on Permutations - multiset permutations
-- n! / (x1! * x2! * x3 ...), n = # of digits, xk = # of xk digits
combsWithDigits :: [Int] -> Int
combsWithDigits [] = 1
combsWithDigits (y:ys) =
    let fact n = product [1..n]
        splitter prod length denom accum _ [] = (prod * length) `div` (denom * accum)
        splitter prod length denom accum c (x:xs) = 
            if c /= x
            then splitter (prod * length) (length + 1) (denom * accum) 1 x xs
            else splitter (prod * length) (length + 1) denom (accum+1) c xs
    in splitter 1 1 1 1 y ys
