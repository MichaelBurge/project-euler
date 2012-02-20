module Parsing where
import qualified Prelude
import Prelude hiding (lookup, null)
import Control.Applicative hiding (empty, many)
import Data.Array.IArray(indices, listArray, bounds, Array, (!))
import Data.List ((\\))
import Data.Map (Map, 
                 fromList, 
                 lookup, 
                 size, 
                 mapWithKey, 
                 elems, 
                 foldlWithKey, 
                 null, 
                 empty,
                 findWithDefault
                )
import qualified Data.Map as Map
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

dataFile :: Parser [[Integer]]
dataFile = endBy lineS eol
csvFile = endBy line eol
nameFile = sepBy nameCell (char ',')
lineS :: Parser [Integer]
lineS = sepBy cell (char ' ')
line = sepBy cell (char ',')
cell :: Parser Integer
cell = read <$> many1 digit
nameCell :: Parser String
nameCell = do
    char '"' 
    content  <- many (noneOf ",\n\"") 
    char '"'
    return content
eol = many1 $ oneOf "\n\r"

type TwoDArray = Array (Int, Int) Integer
type Row = Map Int Integer

listToRow :: [a] -> Map Int a
listToRow = fromList . (zip ([0..]::[Int]))

toMaps :: String -> (Parser [[a]]) -> Map Int (Map Int a)
toMaps text par = 
  listToRow $ 
  map listToRow $ 
  either (error . show) id $ 
  parse par "" text

transposeMaps :: Map Int Row -> Map Int Row
transposeMaps xss = 
  let ele i = fromList [(k, (xss Map.! k) Map.! i) | k <- [0..(size xss)-1]]
  in fromList [(k, ele k) | k <- ([0..(size (xss Map.! 0))-1])]

listsToArray :: [[Integer]] -> TwoDArray
listsToArray xss = listArray ((1, 1), (length xss, length $ head xss)) (concat xss)

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

addRow :: Row -> Row -> Row
addRow maxes newElements =
  let ele i = case lookup i maxes of
        Nothing -> 0
        Just a -> a
      f 0 x = (+) x $ ele 0
      f i x | (i+1) == (size newElements) = (+) x $ ele (i-1)
      f i x = (+) x $ max ((ele $ i-1)) (ele i)
  in mapWithKey f newElements

pathMin :: Row -> Row -> Row
pathMin p n = 
  let ele = Map.findWithDefault 0
      f 0 x = (+) x $ ele 0 p
      f i x = (+) x $ min (ele i p) (f (i-1) (ele (i-1) n))
  in Map.mapWithKey f n
     
pathMin2 :: Row -> Row -> Row
pathMin2 p n = 
  let ele = Map.findWithDefault 0
      g 0 (-1) x = ele 0 x
      g i 1 x | i == ((size n) - 1) = ele i x
      g i j x = (+) (ele i x) $ min (ele i p) (g (i+j) j n)
      f i x = minimum [ele i p,
                       g i (-1) n,
                       g i 1 n
                      ]
  in Map.mapWithKey f n

adjustFirstRow :: Row -> Row
adjustFirstRow row = fromList $ map (\i -> (i, Map.fold (+) 0 $ Map.filterWithKey (\k _ -> k <= (fromIntegral i)) row)) ([0..(size row)-1]::[Int])
 
finals :: (Row -> Row -> Row) -> Map Int Row -> [Integer]
finals g rows = elems $ foldlWithKey (\b k a -> g b a) Map.empty rows
maxes rows = finals addRow rows
mins rows = finals pathMin (Map.updateWithKey (\i a -> Just (adjustFirstRow a)) 0 rows)
mins2 rows = finals pathMin2 rows