import Common.Digits
import Common.Matrix
import Common.Sequence
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

inverses = map (1-)

uniqNonSubsBy preds size trials =  
  let f [] a = [a]
      f (p:ps) a = do
        x <- [a, p a]
        x:(f ps x)
  in nubBy (\x y -> any (x==) $ 
                    f preds y
           ) $ nonSubs size trials

uniqNonSubs size trials =
  -- Exclude exact inverses (a ~ map (1-) a). Need proof of this.
  -- Exclude reverses (a ~ reverse a). Works, because (map t [0..2^n-1] == reverse $ map t [2^n..2^(n+1)]), so a is valid iff reverse a is valid.
  -- 
  let r a b = any (a==) [b, 
                         map (1-) b,
                         reverse b,
                         map (1-) $ reverse b
                        ]
  in nubBy r $ 
     nonSubs size trials
     
diffLists [] = []
diffLists xs = let diffList [] = []
                   diffList l = zipWith (-) (tail l) l 
               in xs:(diffLists $ diffList xs)

-- 'strong cubes initial' as in strongly cubefree using the first element
scubesi [] = []
scubesi (x:xs) = do
  as <- filter (\s -> 
                 ((length s) * 2 + 3) <= 
                 ((1+) $ length xs)
               ) $ 
        inits xs
  let c = (x:as) ++ 
          (x:as) ++ 
          [x]
  if all id $
     zipWith (==) c (x:xs)
    then return c
    else []

scubes [] = []
scubes xs = (scubesi xs) ++ (scubes $ tail xs)

-- Don't use - duplicate logic in scubes and not sure if needed.
nonCubes [] = []
nonCubes (x:xs) = do
  as <- takeWhile (\s -> 
                    ((length s) * 2 + 3) <= 
                    ((1+) $ length xs)
                  ) $ 
        inits xs
  return $ if all id $
              zipWith (==) ((x:as) ++ 
                            (x:as) ++ 
                             [x]
                           ) (x:xs)
           then 0
           else 1

isCubeSeq :: (Eq a) => [a] -> Bool
isCubeSeq = not . null . scubes

totalScubes n = 
  length $ 
  nub $ 
  concat $ 
  map scubes $ 
  map (toDigits 2) [0..n]

-- TODO: Verify this against PE
a :: Int -> Integer -> Integer
a _ 0 = 0
a trials n = 
  let prev :: Integer 
      prev = a trials $ n-1
  in head $
     dropWhile (\x -> not $ isSubOf (reverse . toDigits 2 $ x) 
                      tSeq 
                      trials
               ) $ 
     [(prev+1)..]

nonCubesInA trials n = 
  filter (not . isCubeSeq) $ 
  map (reverse . toDigits 2) $ 
  (takeWhile (<n) $ 
   (map (a trials) [1..])
  ) \\ [1..n]



main =  putStrLn $
        show $ 
        nonCubesInA 200 10000

-- TODO: Verify with PE that a is valid, and that nonCubesInA is correct.
-- TODO: Then, the only exceptions are strong cubes in binary, so need to
-- compute the number of binary numbers that are strongly cubefree.
-- TODO: Figure out a way to handle the power of 10 boundary, since our soluti
-- -ion would work for powers of 2 naturally.


numNonCubes [] = 0          
numNonCubes (x:xs) = 
  let i = numNonCubes xs
  in do as <- inits xs
        let bs = (x:as) ++ 
                 (x:as) ++
                 [x]
        return $
