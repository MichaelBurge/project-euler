{-# LANGUAGE TupleSections #-}

import Common.Sequence
import Data.Char
import Data.List

smallTriangles = takeWhile (<10000) triangles

od c = (ord c) - (ord 'A') + 1

triangleWord :: String -> Bool
triangleWord s =
  let n = sum $ map od s
  in case lookup (fromIntegral n) $ map (,True) smallTriangles of
       Just x -> True
       Nothing -> False

main = do
  contents <- readFile "42.txt"
  putStrLn $ show contents
  let ls = lines contents
  let triangleWords = filter triangleWord ls
  putStrLn $ show $ length triangleWords