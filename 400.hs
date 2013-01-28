import Common.Trees
import Data.Functor
import System.Environment

main = do
  args <- getArgs
  let nums = map read args
  putStrLn $ drawNumTrees $ map fibTree nums