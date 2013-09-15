{-
a * (3a-1)/2 + b * (3b-1)/2 = c * (3c-1)/2
a * (3a-1)/2 - b * (3b-1)/2 = d * (3d-1)/2
a * (3a-1) = c * (3c-1)/2 + d * (3d-1)/2
b * (3b-1) = c * (3c-1)/2 - d * (3d-1)/2

a * (3a-1) = Pc + Pd
b * (3b-1) = Pc - Pd

for each b, check n of the as for a match
-}
import Common.Sequence
import Data.Set
import Safe

pents = fromAscList $ take 10000 $ pentagonals

isGood :: Integer -> Integer -> Bool
isGood a b = ((a + b) `member` pents) &&
             ((a - b) `member` pents)

f = do
  a <- elems pents
  b <- elems pents
  if isGood a b
     then return $ abs (a - b)
     else []

answer = minimumDef (-1) f

main = putStrLn $ show answer