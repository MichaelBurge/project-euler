module Common.Tests.Prime (primesTests) where

import Common.Prime (prime, primes)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Data.Digits (digits)

test_primes = assert $ takeWhile (<20) primes == [2,3,5,7,11,13,17,19]
test_prime = assert $ all prime [19, 211, 877, 27644437]

primesTests =
  testGroup "Primes" [
      testCase "primes" test_primes,
      testCase "prime" test_prime
  ]