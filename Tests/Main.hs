import Test.Framework (defaultMain)
import Common.Tests.Digits (digitsTests)
import Common.Tests.Prime  (primesTests)
import Common.Tests.Sequence (sequenceTests)

main = defaultMain tests
tests = [digitsTests, primesTests, sequenceTests]