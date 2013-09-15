fib 0 = 1
fib 1 = 1
fib n = (fib $ n-1) + (fib $ n-2)

termsTo n = takeWhile (< n) $ map fib [1..]

answer n = sum $ filter even $ termsTo n