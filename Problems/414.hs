import Common.Digits
import qualified Data.HashTable as HT
import Data.List
import System.IO.Unsafe

kaprekate digits base n =
    let digs = sort $ pad digits (toDigits base n) 0
    in kapreDigs digs base
kapreDigs digits base =
    (fromDigits base digits) - (fromDigits base $ reverse digits)

isKaprekar digits base n = (kaprekate digits base n) == n

isBoring b n = let
    digs = pad 5 (toDigits b n) 0
    in and $ (length digs == 5):(map (== head digs) (tail digs))

memoize :: (Integer -> Integer) -> IO (Integer -> Integer)
memoize f = do
    cache <- HT.new (==) (HT.hashInt . fromIntegral)
    return (\n -> unsafePerformIO $ do
              cached <- HT.lookup cache n
              case cached of
                Just v -> return v
                Nothing -> do
                            let v = f n
                            HT.insert cache n v
                            return v )


_S b = let
    _K :: Integer -> Integer
    _K = unsafePerformIO $ memoize (kaprekate 5 b)
    isK k = k == (_K k)
    ks n = takeWhile (not . isK) $
           iterate _K n
    c = _K $ last $ ks 1
    
    iters = unsafePerformIO $ memoize (\n -> 
                     if (n == c) || (isBoring b n)
                     then 0
                     else 1 + (iters $ _K n))
    -- descending returns in sorted order, not the digit itself
    compensator = if (fromDigits b $ reverse $ sort $ toDigits b c) == c
                  then 0
                  else 1
    rawResults = map (\xs -> (fromIntegral $ combsWithDigits xs) *
                        (iters $ fromDigits b $ map fromIntegral xs)) $
            descendings (fromIntegral b) 5
    list2 = map iters [1..b^5-1]
  in (sum rawResults) - compensator
main = putStrLn $ show $ _S 33