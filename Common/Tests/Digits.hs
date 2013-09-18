module Common.Tests.Digits (digitsTests) where

import Common.Digits (fromDigits, toDigits, foldDigits)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Digits (digits)

prop_fromDigits_toDigits_are_inverses b (Positive n) =
    (b :: Integer) > 1 ==>
        (n :: Integer) == (fromDigits b $ toDigits b n)

prop_toDigits b (Positive n) =
    (b :: Integer) > 1 ==>
        (digits b (n :: Integer)) == (toDigits b n)

prop_toDigits_as_a_fold b (Positive n) =
    (b :: Integer) > 1 ==>
         (toDigits b n) == foldDigits b (\xs x -> (fromIntegral x):xs) [] (n :: Integer)

digitsTests =
  testGroup "Digits" [
    testProperty "fromDigits . toDigits" prop_fromDigits_toDigits_are_inverses,
    testProperty "toDigits" prop_toDigits,
    testProperty "toDigits can be implemented using foldDigits" prop_toDigits_as_a_fold
  ]

