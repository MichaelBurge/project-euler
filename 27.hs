import Data.List
import Prime
import Control.Parallel.Strategies
import Control.Parallel

polyVals a b = map (\x -> (x*x) + (a*x) + b) [0..]

main = putStrLn $
       show $
       maximumBy (\(_,_,a) (_,_,b) -> compare a b) $ 
       map (\(a,b,c) -> (a,b, length . (takeWhile prime) $ c)) $ 
       [(a,b,polyVals a b)| a <- [-999..999], b <- [-999..999]]