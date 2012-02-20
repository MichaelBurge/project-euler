import Prime

rcheck x y = odd $ floor $ (y*y) / (x*x)
reference m n = length $ filter id $ [rcheck x y| x <- [m..n], y <- [m..n]]

check f = (f 0 100) == 3019