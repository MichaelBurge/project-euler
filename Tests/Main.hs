import Test.Framework (defaultMain)
import Common.Tests.Digits (digitsTests)
import Common.Tests.Prime  (primesTests)

main = defaultMain tests
tests = [digitsTests, primesTests]