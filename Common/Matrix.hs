module Common.Matrix where

eleAt 0 (x:xs) = x
eleAt n (x:xs) = eleAt (n-1) xs

dropAt 0 (x:xs) = xs
dropAt n (x:xs) = x:(dropAt (n-1) xs)

minor xss n = tail (map (dropAt n) xss)

permanent :: [[Integer]] -> Integer
permanent [] = 1
permanent xss = sum $
  zipWith (\n ys -> 
            (eleAt n (head xss)) *
            (permanent $ 
             minor xss n
            ) 
          )
    [0..] xss

restrictedPermanent [] = 1
restrictedPermanent [[x]] = x
restrictedPermanent xss = 
  let size = length xss
      a = size `div` 2
      b = size - a
      sub f g = restrictedPermanent $ map f $ g xss
  in (sub (take a) (take a)) * (sub (drop a) (drop a)) +     
     (sub (take b) (drop a)) * (sub (drop b) (take a))

constMat k n = take n $ map (take n) $ repeat . repeat $ k
ones = constMat 1