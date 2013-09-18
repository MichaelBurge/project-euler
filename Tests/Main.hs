import Test.Framework (defaultMain)
import Common.Tests.Digits (digitsTests)

main = defaultMain tests

tests = [digitsTests]