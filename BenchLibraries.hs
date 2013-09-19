module Main where

import Control.DeepSeq(NFData)
import Criterion.Config
import Criterion.Main
import Common.Digits(foldDigits, fromDigits, toDigits)
import Common.Prime(factors, prime)
import Data.Digits(digitsRev, unDigits)
import Types

bigNumber :: Integer
bigNumber = 1234567891011121314151617181920
numToFactor :: Integer
numToFactor = 1578292929

bigPrime :: Integer
bigPrime = 5754853343

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
        -- baseBench "foldDigits" foldDigitsTest,
        -- bcompare $ [
        --   benchNParam "digits" digitsRev [2] bigNumber,
        --   benchNParam "toDigits" toDigits [2] bigNumber
        -- ],
        -- bcompare $ [
        --   benchNParam "digits" digitsRev [10] bigNumber,
        --   benchNParam "toDigits" toDigits [10] bigNumber
        -- ],
        -- baseBench "to-and-from-digits" toAndFromTest,
        -- let testDigits = toDigits 3 bigNumber
        --   in bcompare [
        --    bench "undigits 3" $ nf (unDigits 3 . reverse) testDigits,
        --    bench "fromDigits 3" $ nf (fromDigits 3) testDigits
        -- ],
        bench "factors" $ nf factors numToFactor,
        bench "prime" $ nf prime bigPrime
       ]
