module Main where

import           Data.Char (isDigit, isSpace)
 -- Extract numbers from a string

getNumbers "" = []
getNumbers x =
  if length first > 0
    then (read :: String -> Int) first : getNumbers second
    else getNumbers second
  where
    (first, second) = span isDigit . dropWhile (not . isDigit) $ x
 -- Parses a pattern of the type (AxB)XXXYYY and returns (A, B, XXX, YYY)

parsePattern :: String -> (Int, Int, String, String)
parsePattern xs = (a, b, first, second)
  where
    (digits, rest) = span (/= ')') xs
    [a, b] = getNumbers digits
    (first, second) = splitAt a (tail rest)
 -- Decompresses an (AxB)XXX compressed string

decompress :: String -> String
decompress "" = ""
decompress ('(':xs) = (concat . replicate b) first ++ decompress second
  where
    (a, b, first, second) = parsePattern xs
decompress xs = first ++ decompress second
  where
    (first, second) = span (/= '(') xs
 -- Solution to the first star

star1 :: String -> Int
star1 = length . decompress . filter (not . isSpace)
 -- Computes the length of the v2-decompressed string

decompressLength "" = 0
decompressLength ('(':xs) =
  (decompressLength first) * b + decompressLength second
  where
    (a, b, first, second) = parsePattern xs
decompressLength (x:xs) = 1 + decompressLength xs
 -- Solution to the second star

star2 :: String -> Int
star2 = decompressLength . filter (not . isSpace)

main :: IO ()
main = do
  print $ decompress "(1x2)AB" == "AAB"
  print $ decompress "A(1x5)BC" == "ABBBBBC"
  print $ decompress "(3x3)XYZ" == "XYZXYZXYZ"
  print $ decompress "A(2x2)BCD(2x2)EFG" == "ABCBCDEFEFG"
  print $ decompress "(6x1)(1x3)A" == "(1x3)A"
  print $ decompress "X(8x2)(3x3)ABCY" == "X(3x3)ABC(3x3)ABCY"
  print $ decompressLength "X(8x2)(3x3)ABCY" == length "XABCABCABCABCABCABCY"
  print $ decompressLength "(27x12)(20x12)(13x14)(7x10)(1x12)A" == 241920
  print $
    decompressLength "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" ==
    445
  s <- readFile "input"
  print $ star1 s
  print $ star2 s
--
