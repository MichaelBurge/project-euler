module Common.Trees where
import Data.Tree

-- Gives each node the function height it was constructed with
fibTreeDepth :: Integer -> Tree Integer
fibTreeDepth 0 = Node 0 []
fibTreeDepth 1 = Node 1 [fibTreeDepth 0]
fibTreeDepth n = Node n [fibTreeDepth (n-1), fibTreeDepth (n-2)]

-- Gives each node a unique integer
fibTree :: Integer -> Tree Integer
fibTree 0 = Node 0 []
fibTree 1 = Node 0 [fmap (+1) $ fibTree 0]
fibTree n = 
    let t1 = fibTree $ n-1
        t1Size = fromIntegral $ length $ flatten t1
    in Node 0 [
            fmap (+1) $ fibTree (n-1),
            fmap (+(t1Size + 1)) $ fibTree (n-2)
           ]

drawNumTree :: Tree Integer -> String
drawNumTree = drawTree . fmap show

drawNumTrees :: Forest Integer -> String
drawNumTrees = drawForest . fmap (fmap show)

delete :: Integer -> Tree Integer -> Tree Integer
delete n (Node _ [xs]) = filter (\(Node x _) -> x /= n) xs




-- Moves allowed are removing any subtree. Objective is to make your opponent remove the last tree.
-- Slow because subtrees are counted many many times(one for each parent). Can be reduced by deciding to take the root or a child, and narrowing down under the list monad.
isWinningMove :: Tree a -> Integer -> Bool
isWinningMove (Node x []) _ = False
isWinningMove t@(Node a xs) n =
    let reduced = delete n t
    in all (not . isWinningMove) $ tail $ flatten t

