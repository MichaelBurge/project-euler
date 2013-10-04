module Problems.Problem59 where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List.Split
import System.IO.Unsafe
import Types

commonEnglishWords =
    [
     "a",
     "the"
    ]

ciphertext = map (chr . read) $ splitOn "," $ unsafePerformIO $ readFile "Problems/Problem59.txt"
applyCipher key = map chr $ zipWith (on xor ord) ciphertext $ cycle key

validKeys = do
    let ls = ['a'..'z']
    a <- ls
    b <- ls
    c <- ls
    let key = [a,b,c]
    let decrypted = applyCipher key
    if all (`elem` words decrypted) commonEnglishWords
      then return key
      else []

correctKey = head validKeys
answer = sum $ map ord $ applyCipher correctKey

problem59 :: Problem
problem59 = Problem {
    description = "Problem 59: XOR decryption",
    solution = show answer
}