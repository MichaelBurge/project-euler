module Common.Tests.Prime (primesTests) where

import Common.Prime (modExp, prime, primeByPrp, primeByFactoring, primePower, primes)
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

primesTests =
  testGroup "Primes" [
      testCase "primes" test_primes,
      testCase "prime" test_prime,
      testProperty "Fermat's Little Theorem" prop_fermat_little_theorem,
      testProperty "PRP primality test" prop_primeByPrp_iff_primeByFactoring,
      testCase "primePower" test_primePower
  ]