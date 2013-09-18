module Main where

import Criterion.Config
import Criterion.Main
import Common.Digits(foldDigits, fromDigits, toDigits)
import Types

bigNumber :: Integer
bigNumber = 1234567891011121314151617181920

main = Criterion.Main.defaultMain $
       [
        bench "foldDigits 10" $ nf (foldDigits 10 (\xs x -> x:xs) []) bigNumber,
        bench "foldDigits 2"  $ nf (foldDigits 2 (\xs x -> x:xs) []) bigNumber,
        bench "toDigits 2"  $ nf (toDigits 2 ) bigNumber,
        bench "toDigits 10" $ nf (toDigits 10) bigNumber,
        bench "to-and-from digits 10" $ nf (fromDigits 10 . map (\x -> x + 1 `quot` 10) . toDigits 10) bigNumber,
        bench "to-and-from digits 2" $ nf (fromDigits 2 . map (\x -> x + 1 `quot` 2) . toDigits 2) bigNumber
       ]
