import Data.Function

optimal 1 = 1
optimal n = minimum $ map (\i -> (optimal i) + (optimal $ n - i)) [1..n - 1]