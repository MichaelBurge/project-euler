next (n,a,b,c,d) = (n+8,a+n+2,b+n+4,c+n+6,d+n+8)
main = putStrLn $ 
       show $ 
       (foldl' (+) 0 $ map (\(n,a,b,c,d) -> a + b + c + d) $ 
        take 501 $ 
        iterate next (0,1,1,1,1)
       ) - 3