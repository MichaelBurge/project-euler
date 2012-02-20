import Control.Monad
import Data.List
import Debug.Trace
import Prime
import Digits

main = putStrLn $
       show $
       map (fromDigits 10) $
       map head $
       filter (all (prime . fromDigits 10)) $
       map rotations $
       map (toDigits 10) $
       filter prime $
       [1..1000000]