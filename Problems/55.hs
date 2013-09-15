import Digits

isLyc _ 0 = True
isLyc n t = let new = (n + (fromDigits 10 $ reverse $ toDigits 10 n))
            in (not . isPalindrome 10 $ new) && isLyc new (t-1) 