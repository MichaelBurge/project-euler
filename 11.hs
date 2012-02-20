import Control.Applicative
import Data.Array.IArray
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

dataFile = endBy line eol
line = sepBy cell (char ' ')
cell :: Parser Integer
cell = read <$> many1 digit
eol = char '\n'

type TwoDArray = Array (Int, Int) Integer

dataArray :: [[Integer]] -> TwoDArray
dataArray xss = listArray ((1, 1), (length xss, length $ head xss)) (concat xss)

elementAt :: TwoDArray -> (Int, Int) -> Integer
elementAt arr (x, y) = 
  let bnds = bounds arr
      l = fst . fst $ bnds
      r = fst . snd $ bnds
      t = snd . fst $ bnds
      b = snd . snd $ bnds
      check = l <= x && x <= r &&
              t <= y && y <= b
  in if check
       then arr ! (x,y)
       else 0
            
dirProd :: TwoDArray -> (Int, Int) -> (Int, Int) -> Int -> Integer
dirProd arr p d 0 = 1
dirProd arr p d n = (elementAt arr p) * (dirProd arr ((fst p) + (fst d), (snd p) + (snd d)) d (n-1))      

largestProductAt :: TwoDArray -> (Int, Int) -> Int -> Integer
largestProductAt arr pos n = 
  let dirs = [(x,y) | x <- [-1..1], y <- [-1..1]] \\ [(0,0)]
  in maximum $ map (\x -> dirProd arr pos x n) dirs 
     
largestProduct arr n = maximum $ map (\x -> largestProductAt arr x n) (indices arr)