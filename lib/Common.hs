module Common where

import Data.Char (isDigit)

getNumbers :: Read a => String -> [a]
getNumbers "" = []
getNumbers x =
  if length first > 0
    then read first : getNumbers second
    else getNumbers second
  where
    (first, second) = span isDigit . dropWhile (not . isDigit) $ x