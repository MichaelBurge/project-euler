module Common.String where
import Data.List
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

numOf cs c = length $ filter (==c) cs

replace :: Char -> Char -> String -> String
replace f t s = map (\x -> if x == f then t else x) s

replaces :: String -> String -> String -> String
replaces from to source = 
  let convChar c = case lookup c (zip from to) of
                     Just b -> b
                     Nothing -> c
  in map convChar source

numDiffering :: String -> String -> Int
numDiffering a b = length $ filter (\x -> fst x /= snd x) $ zip a b

posSubstrs :: String -> String -> [Int]
posSubstrs substr [] = []
posSubstrs substr a@(x:xs) =
    let isInitial ys = (length ys >= length substr) &&
                       all id (zipWith (==) substr ys)
    in (map (1+) (posSubstrs substr xs)) ++
       if isInitial a
       then [1]
       else []