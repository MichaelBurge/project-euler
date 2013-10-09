module Common.Tests.Prime (primesTests) where

import Common.Prime
    (
     modExp,
     prime,
     primeByPrp,
     primeByFactoring,
     primePower,
     primes,
     rabin_miller
    )
import System.Random
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck


test_primes = assert $ takeWhile (<20) primes == [2,3,5,7,11,13,17,19]
test_prime = assert $ all prime [19, 211, 877, 27644437]

prop_fermat_little_theorem (Positive n) (Positive a) =
    (n > 2) ==> 
    forAll (elements (takeWhile (<n) primes)) $ \p ->
        (modExp p a p) == (a `rem` p)

prop_primeByPrp_iff_primeByFactoring (Positive n) =
    primeByPrp n == primeByFactoring n

test_primePower =
    assert $
    map (primePower (5^4 * 7^3 * 2^7)) [2,3,5,7] ==
    [7, 0, 4, 3]

inRange :: Integer -> Integer -> Bool
inRange x b = x > 1 && x < b

prop_rabinMiller_no_false_negatives n x = (n `inRange` 1000000) ==>
    let smallPrimes = takeWhile (<500) primes
    in forAll (elements smallPrimes) $ \p ->
        rabin_miller (mkStdGen x) p

-- It is theoretically possible for this test to fail, but it shouldn't.
prop_rabinMiller_iff_prime n x = (n `inRange` 1000000) ==>
    rabin_miller (mkStdGen x) n == prime n

primesTests =
  testGroup "Primes" [
      testCase "primes" test_primes,
      testCase "prime" test_prime,
      testProperty "Fermat's Little Theorem" prop_fermat_little_theorem,
      testProperty "PRP primality test" prop_primeByPrp_iff_primeByFactoring,
      testCase "primePower" test_primePower,
      testProperty "Rabin-Miller - Doesn't complain about primes" prop_rabinMiller_no_false_negatives,
      testProperty "Rabin-Miller - Exact primality(possibly fails)" prop_rabinMiller_iff_prime
  ]