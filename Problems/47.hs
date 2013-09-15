import Common.Prime
import Data.List(find)
import Data.Set
consecs :: Integer
consecs = 4
check n = (==) (fromIntegral consecs) $ size $ primeFactors n
bound :: Integer
bound = 240000
checks = fromAscList $ Prelude.filter check [1..bound]

isGood x = all (\k -> (x + k) `member` checks) [0..consecs-1]

answer = case find isGood [1..bound] of
           Just x -> x
           Nothing -> -1

answers = Prelude.filter isGood [1..bound]

--main = putStrLn $ show $ isGood 238203
main = putStrLn $ show answer
--main = putStrLn $ unlines $ Prelude.map (\k -> show $ (k, primeFactors k)) [1..bound]