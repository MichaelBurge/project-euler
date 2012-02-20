import Data.List

digitList b 0 = []
digitList b n = (n `rem` b) : (digitList b $ n `quot` b)

isPalindrome b n = (\x -> x == (reverse x)) (digitList b n)

answer = sort $ filter (isPalindrome 10) [x * y | x <- [100..999], y <- [100..999]]