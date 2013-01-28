import Common.Digits

lower :: Integer
--lower = 1020304050607080900
lower   = 1020304050607080900
upper :: Integer
--upper = 1929394959697989990
upper   = 1929394959697989990
isGood x = 
    let check n i = (x `div` 100^n) `rem` 10 == i
    in check 0 0 &&
       check 1 9 &&
       check 2 8 &&
       check 3 7 &&
       check 4 6 &&
       check 5 5 &&
       check 6 4 &&
       check 7 3 &&
       check 8 2 &&
       check 9 1

start :: Integer
start = floor $ sqrt $ fromIntegral lower
candidates = filter (>= lower) $ takeWhile (<= upper) $ map (^2) [start..]
prune xs = filter isGood xs
main = putStrLn $ show $ prune candidates