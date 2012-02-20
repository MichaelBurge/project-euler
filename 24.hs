import Data.List

nextPerm :: [Integer] -> Maybe [Integer]
nextPerm [] = Nothing
nextPerm (x:xs) = case nextPerm xs of
  Nothing -> let gs = filter (>x) xs
                 a = minimum gs
             in if null gs
                  then Nothing
                  else Just $ a:(sort(x:(xs \\ [a]))) 
  Just ys -> Just $ x:ys