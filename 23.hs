import Prime
import Data.List
import qualified Data.Set as Set

main = putStrLn $ 
       show $ 
       foldl' (+) 0 $
       Set.toList $
       Set.difference (Set.fromList [1..28123]) $
       sumsFrom 28124 $
       Set.toList $
       abundants 28124