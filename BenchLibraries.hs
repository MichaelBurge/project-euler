module Main where

import Control.DeepSeq(NFData)
import Criterion.Config
import Criterion.Main
import Common.Digits(foldDigits, fromDigits, toDigits)
import Types

bigNumber :: Integer
bigNumber = 1234567891011121314151617181920
benchNParam :: NFData b => String -> (Integer -> a -> b) -> [Integer] -> a -> Benchmark
benchNParam name f bs n = 
    bgroup name $
    map (\b -> bench (show b) $
               nf (f b) n
    ) bs
baseBench name f = benchNParam name f [2,10] bigNumber

foldDigitsTest b = foldDigits b (\xs x -> x:xs) []
toAndFromTest b = fromDigits b . map (\x -> fromIntegral $ x + 1 `quot` b) . toDigits b

main = Criterion.Main.defaultMain $
       [
        baseBench "foldDigits" foldDigitsTest,
        baseBench "toDigits" toDigits,
        baseBench "to-and-from-digits" toAndFromTest,
        let digits = toDigits 3 bigNumber
        in bench "fromDigits 3" $ nf (fromDigits 3) digits

       ]
