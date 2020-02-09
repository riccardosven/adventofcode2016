module Main where

import           Data.Hashable
import qualified Data.HashMap.Lazy as Map hiding (foldr)
import           Data.Maybe

-- Counter type is a HashMap with integer keys
type Counter a = Map.HashMap a Int-- Update the counter with the key a, default lookups are 0

counterUpdate :: (Eq a, Hashable a) => a -> Counter a -> Counter a
counterUpdate k map = Map.insert k (v + 1) map
  where
    v = Map.lookupDefault 0 k map
 -- Return counts by column of a string

colCounts' :: (Eq a, Hashable a) => [Counter a] -> [a] -> [Counter a]
colCounts' [] s = map (flip counterUpdate Map.empty) s
colCounts' c s  = zipWith counterUpdate s c
 -- Counts by column of a set of strings

colCounts :: (Eq a, Hashable a) => [[a]] -> [Counter a]
colCounts = foldr (flip colCounts') []
 -- Most common element in a Counter

mostCommon :: (Eq a, Hashable a) => Counter a -> Maybe (a, Int)
mostCommon m = foldr go Nothing (Map.toList m)
  where
    go x Nothing = Just x
    go (k1, v1) (Just (k2, v2)) =
      if v1 > v2
        then Just (k1, v1)
        else Just (k2, v2)
 -- Least common element in a Counter

leastCommon :: (Eq a, Hashable a) => Counter a -> Maybe (a, Int)
leastCommon m = foldr go Nothing (Map.toList m)
  where
    go x Nothing = Just x
    go (k1, v1) (Just (k2, v2)) =
      if v1 < v2
        then Just (k1, v1)
        else Just (k2, v2)

-- Solution to the first star
star1 :: String -> String
star1 = map (fst . fromJust . mostCommon) . colCounts . lines

-- Solution to the second star
star2 :: String -> String
star2 = map (fst . fromJust . leastCommon) . colCounts . lines

main :: IO ()
main = do
  s <- readFile "testinput"
  print $ star1 s
  print $ star2 s
  s <- readFile "input"
  print $ star1 s
  print $ star2 s
