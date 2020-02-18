module Common where

import Data.Char (isDigit)
import qualified Data.ByteString.Char8      as BS
import           Crypto.Hash.MD5 (hash)
import           Data.Char (ord)

getNumbers :: Read a => String -> [a]
getNumbers "" = []
getNumbers x =
  if length first > 0
    then read first : getNumbers second
    else getNumbers second
  where
    (first, second) = span isDigit . dropWhile (not . isDigit) $ x

-- Decodes unicode characters (16 bit) from the ByteString into pairs of 8bit integers
decodeChars :: [Char] -> [Int]
decodeChars [] = []
decodeChars (x:xs) = c1 : c2 : decodeChars xs
  where
    (c1, c2) = divMod (ord x) 16

-- Computes the hash of the input string
computeHash :: String -> [Int]
computeHash = decodeChars . BS.unpack . hash . BS.pack

