import Data.List

divDigits n x =
  let val = (10*n) `div` x 
  in (n,val) : (divDigits ((10*n) - x * val) x)
     
initialCycle n =
  takeWhile (\x -> (length x) == (length $ nub x)) $ inits $ divDigits 1 n


repeatingCycle n =
  let rep = last $ head $ dropWhile (\x -> (length x) == (length $ nub x)) $ inits $ divDigits 1 n
      f = (/=) rep
  in map snd $ (:) rep $ takeWhile f $ tail $ dropWhile f $ divDigits 1 n

biggestCycleTo n =
  maximum $ map length $ map repeatingCycle [1..n]