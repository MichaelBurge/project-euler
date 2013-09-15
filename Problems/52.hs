import Digits
import Data.List

chain n = takeWhile (\x -> null $ (toDigits 10 n) \\ (toDigits 10 $ x * n)) [1..]