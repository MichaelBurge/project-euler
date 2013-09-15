import Common.Digits

-- t_0 = 0
-- t_n = t_(n `div` 10) + t_(n `div` 100)

t n = 0 : (concat $ map (reverse . toDigits 10) [1..n])

answer =
  let list = t 10000001
  in list !! 12
--  in map (\x -> list !! (10^x)) $ [1..7]

main = putStrLn $ show answer