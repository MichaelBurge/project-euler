import Data.List

partitions 0 = [[]]
partitions n = nub $ map (reverse.sort) $ concatMap (\x -> map ((n-x):) $ partitions x) [0..n-1]

