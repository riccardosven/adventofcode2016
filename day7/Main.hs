module Main where

import           Data.List  (isInfixOf)
 -- Find if there is an ABBA tag in the string
import           Data.Maybe

abbaTag :: String -> Maybe String
abbaTag x
  | length x < 4 = Nothing
  | x !! 0 == x !! 3 && x !! 1 == x !! 2 && x !! 0 /= x !! 1 = Just $ take 4 x
  | otherwise = abbaTag $ tail x
 -- Split the ip in net and subnet portions

splitNetSubnet :: String -> ([String], [String])
splitNetSubnet "" = ([], [])
splitNetSubnet xs = (outer : first, inner : second)
  where
    (outer, inner') = span (/= '[') xs
    (inner, rest) = span (/= ']') . dropWhile (== '[') $ inner'
    (first, second) = splitNetSubnet . dropWhile (== ']') $ rest
 -- Check an Ip for TLS compatibility

checkTls :: String -> Bool
checkTls ip = any (isJust . abbaTag) net && not (any (isJust . abbaTag) subnet)
  where
    (net, subnet) = splitNetSubnet ip
 -- Solution to the first star

star1 :: [String] -> Int
star1 = length . filter (checkTls)
 -- Return list of ABA tags

abaTags :: String -> [String]
abaTags x
  | length x < 3 = []
  | x !! 0 == x !! 2 && x !! 0 /= x !! 1 = (take 3 x) : (abaTags (tail x))
  | otherwise = abaTags $ tail x
 -- Check if there is an ABA tag with a matching BAB tag

checkSsl :: String -> Bool
checkSsl ip = any (babcheck subnet) abalist
  where
    abalist = concatMap abaTags net
    babcheck subnet aba =
      any (isInfixOf ((\(x:y:z:[]) -> y : x : y : []) aba)) subnet
    (net, subnet) = splitNetSubnet ip
 -- Solution to the second star

star2 :: [String] -> Int
star2 = length . filter (checkSsl)

main :: IO ()
main = do
  print $ True == checkTls "abba[mnop]qrst"
  print $ False == checkTls "abcd[bddb]xyyx"
  print $ False == checkTls "aaaa[qwer]tyui"
  print $ True == checkTls "ioxxoj[asdfgh]zxcvbnckTls"
  s <- readFile "input"
  print $ star1 . lines $ s
  print $ True == checkSsl "aba[bab]xyz"
  print $ False == checkSsl "xyx[xyx]xyx"
  print $ True == checkSsl "aaa[kek]eke"
  print $ True == checkSsl "zazbz[bzb]cdb"
  print $ star2 . lines $ s
