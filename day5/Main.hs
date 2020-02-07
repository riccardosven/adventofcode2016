module Main where

import Numeric (showHex)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Crypto.Hash.MD5


decodeChars::[Char] -> [Int]
decodeChars [] = []
decodeChars (x:xs) = c1: c2:decodeChars xs
    where
        (c1, c2) = divMod (ord x) 16
    
computeHash :: String -> String
computeHash = concat. map (flip showHex "") . decodeChars . BS.unpack . hash . LBS.toStrict .  LBS.pack

-- findPass :: Int -> String -> String
findPass :: String -> Int -> Int -> String -> String
findPass _ _ 8 acc =  acc -- Return accumulator when we have reached length 8
findPass s n m acc = if take 5 w == "00000"
    then
        findPass s (n+1) (m+1) (acc++[w!!5])
    else
        findPass s (n+1) m acc
    where
        w = computeHash (s ++ show n)

star1::String -> String
star1 x = findPass x 0 0 ""

main = do
    print $  star1 "abc"
    print $  star1 "reyedfim"
