import Common.Digits
import Common.Prime
import Data.Function
import Data.List

seed = [3,7,109,673]
--seed = [3,7,109]
_concat = ((fromDigits 10 . reverse) .) .
          ((++) `on` (toDigits 10))

-- Ex. 673 is an extension of [3,7,109]
isExtension x = all id $ do
  a <- x:seed
  b <- (x:seed) \\ [a]
  return $ prime $ a `_concat` b

firstExt = head $ filter isExtension $ dropWhile (<=673) primes

main = putStrLn $ show firstExt