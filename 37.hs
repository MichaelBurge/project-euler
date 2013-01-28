-- 2,3,5,7 are the only digits on both sides because left-right truncatable both imply the last step is a prime
-- 2 can't be used on the rightmost side
-- 5 can't be used on the rightmost side
-- 2,4,5,6,8 can't be used anywhere except the very left, due to right-truncatability.
-- 4,6,8 can't be used anywhere
-- so only 1,3,7,9 can be used anywhere except the very left.
-- and 1,2,3,5,7,9 can be used on the very left
-- if X is 1 mod 3, 2X and X2 is 0 mod 3, so can't do that.
-- if X is 1 mod 3, 5X and X5 is 0 mod 3, so can't do that.
-- if X is 1 mod 3, 3X and X3 is 1 mod 3, so possible.
-- if X is 1 mod 3, 7X and X7 is 2 mod 3, so possible.
-- if X is 2 mod 3, 7X and X7 is 0 mod 3, so can't do that.

-- Algorithm:
-- Start with 2,3,5,7
-- For each element, append 1,3,7,9 if it's a prime and recurse
-- The tree should terminate because there are only 11 primes.

import Common.Digits
import Common.Prime
f :: (Integer -> Bool) -> [Integer] -> [Integer] -> [Integer]
f pred appends list = do
  x <- list
  a <- appends
  return $ x * 10 + a

g n pred appends list = take n $ map (filter specialPrime) $ iterate (f pred appends) list

-- How many special primes are there in base n?
leftTruncatable :: Integer -> Bool
leftTruncatable n = prime n && (n < 10 || leftTruncatable (fromDigits 10 . reverse . tail . reverse . toDigits 10 $ n))

rightTruncatable n = prime n && (n < 10 || rightTruncatable (n `div` 10))

truncatablePrime :: Integer -> Bool
truncatablePrime n = leftTruncatable n && rightTruncatable n

specialPrime :: Integer -> Bool
specialPrime x = and [
                  truncatablePrime x,
                  (not (x `elem` [2,3,5,7]))
                 ]
--value = filter specialPrime [1..1000000]
value = sum $ concat $ g 6 specialPrime [1,3,7,9] [2,3,5,7]
main = putStrLn $ show value

--main = putStrLn $ show $ 