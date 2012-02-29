import Digits
import Data.List

t 0 = 0
t n = if even n then t $ n `div` 2 else 1 - (t $ n `div` 2)
tSeq =map t [0..]

-- Find a number which only contains at most 2 in a row but isn't a subsequence
atmosts 0 _ = [[]]
atmosts n k = do
  x <- [0..1]
  y <- filter (\zs -> all (<=k) $ map length $ group $ x:zs) $ atmosts (n-1) k
  return $ x:y
  
isSubOf (x:xs) [] _ = False
isSubOf xs ys limit = 
  if limit == 0 
  then False 
  else (all id $ zipWith (==) xs ys) || 
       (isSubOf xs (tail ys) (limit-1))
       
nonSubs size trials = 
  map fst $
  filter (not . snd) $ 
  map (\x -> (x, isSubOf x tSeq trials)) $ 
  atmosts size 2