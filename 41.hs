import Common.Digits
import Common.Prime
import Control.Parallel.Strategies
import Data.List
import Debug.Trace

answer = maximum $
         filter prime $
         parMap rdeepseq (\xs -> fromDigits 10 $ xs ++ [6,7]) $
         permutations [1,2,3,4,5]

main = putStrLn $ show answer