import Prime

isAmicable n = 
  let f n = sum . (filter (<n)) . factors $ n
  in (n == (f . f) n) && (n /= f n)