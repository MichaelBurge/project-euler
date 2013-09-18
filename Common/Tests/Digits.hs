module Common.Tests.Digits (digitsTests) where

import Common.Digits (fromDigits, toDigits, foldDigits, numDigits, replaceDigits)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
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

prop_numDigits_matches_toDigits b (Positive n) =
    (b :: Integer) > 1 ==>
        (length $ toDigits b n) == numDigits b n

test_replaceDigits = replaceDigits 10 2 1234 [True, True, False, True] @?= 2232

digitsTests =
  testGroup "Digits" [
    testProperty "fromDigits . toDigits" prop_fromDigits_toDigits_are_inverses,
    testProperty "toDigits" prop_toDigits,
    testProperty "toDigits can be implemented using foldDigits" prop_toDigits_as_a_fold,
    testProperty "The length of toDigits is numDigits" prop_numDigits_matches_toDigits,
    testCase "replaceDigits" test_replaceDigits
  ]

