module Common.Tests.Sequence (sequenceTests) where

import Common.Sequence (stripUnneeded, subsequenceOf)
import Control.Applicative
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

randomlyInterleave :: [a] -> [a] -> Gen [a]
randomlyInterleave [] ys = pure ys
randomlyInterleave xs [] = pure xs
randomlyInterleave (x:xs) (y:ys) =
    let chooseX = (x:) <$> randomlyInterleave xs (y:ys)
        chooseY = (y:) <$> randomlyInterleave (x:xs) ys
    in oneof [chooseX, chooseY]

prop_subsequences_of_interleaved :: [Integer] -> [Integer] -> Property
prop_subsequences_of_interleaved xs ys =
    forAll (randomlyInterleave xs ys) $
        \zs -> (subsequenceOf xs zs) .&&. (subsequenceOf ys zs)

at_least_3_elements_greater_than_7 xs = 3 <= (length $ filter (>= 7) xs)

test_stripUnneeded =
    assert $
    (==) [ 9, 8, 7 ] $
    stripUnneeded at_least_3_elements_greater_than_7 $
    concat [[ 1 .. 20 ], reverse [1 .. 20]]

sequenceTests =
    testGroup "Sequences" [
        testProperty "Two sequences are subsequences of any interleaved sequence" prop_subsequences_of_interleaved,
        testCase "stripUnneeded" test_stripUnneeded
    ]