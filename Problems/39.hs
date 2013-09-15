import Control.Parallel.Strategies
import Data.Function
import Data.List
import Data.Maybe ( fromJust )
import qualified Data.Map as M

numsquares :: Integer
numsquares = 1000
squares = map (\x -> (x * x, x)) [1..numsquares]
squareMap :: M.Map Integer Integer
squareMap = M.fromList squares

bMap :: M.Map Integer [(Integer,Integer)]
bMap = M.fromList [(a,
                    filter (\x -> snd x >= a) squares)
                   | a <- (map snd squares)
                  ]
answers p = do
  (a2,a) <- squares
  (b2,b) <- case M.lookup a bMap of
                                   Just x -> x
                                   Nothing -> []
  case M.lookup (a2 + b2) squareMap of
    Nothing -> []
    Just c -> if a + b + c == p
              then return (a,b,c)
              else []

answer = maximumBy (compare `on` (length . snd)) $
         parMap rdeepseq (\p -> (p, answers p)) [1..1000]

main = putStrLn $ show answer
