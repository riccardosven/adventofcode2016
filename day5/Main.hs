module Main where

import Common (computeHash)
import           Numeric                    (showHex)

-- Brute forces the password using an accumulator
findPass :: String -> Int -> Int -> String -> String
findPass _ _ 8 acc = acc -- Return accumulator when we have reached length 8
findPass s n m acc =
  if take 5 w == "00000" -- If the hash starts with 5 zeros
    then findPass s (n + 1) (m + 1) (acc ++ [w !! 5]) -- Append the sixth caracter
    else findPass s (n + 1) m acc -- Keep looking
  where
    w = concat . map (flip showHex "") . computeHash $ s ++ show n -- compute hash as string

-- Brute forces the password using an accumulator with insertion at the appropriate positions
findPass' :: String -> Int -> Int -> String -> String
findPass' _ _ 8 acc = acc
findPass' s n m acc =
  if sum (take 5 w) == 0 && wheresplit < 8 && (acc !! wheresplit) == '_' -- If the hash starts with 5 zeros and is a valid index and I have not found this position
    then findPass' s (n + 1) (m + 1) $ first ++ p ++ tail last -- Insert the character
    else findPass' s (n + 1) m acc -- Keep looking
  where
    w = computeHash (s ++ show n)
    p = showHex (w !! 6) "" -- character to insert
    wheresplit = w !! 5 -- where to insert the character
    (first, last) = splitAt wheresplit acc -- split the accumulator so far at the position indicated by the hash

star1 :: String -> String
star1 x = findPass x 0 0 ""

star2 :: String -> String
star2 x = findPass' x 0 0 "________"

main = do
  print $ star1 "abc"
  print $ star2 "abc"
  print $ star1 "reyedfim"
  print $ star2 "reyedfim"
