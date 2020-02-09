module Main where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Read

data Screen =
  Screen [[Bool]]

instance Show Screen where
  show (Screen s) =
    intercalate "\n" $
    map
      (map
         (\x ->
            if x
              then '#'
              else ' '))
      s

 -- Create new empty screen
newScreen :: Int -> Int -> Screen
newScreen x y = Screen $ replicate y (replicate x False)

 -- Get matrix representation from Screen
fromScreen :: Screen -> [[Bool]]
fromScreen (Screen b) = b

 -- rect AxB operation
rect :: Int -> Int -> Screen -> Screen
rect a b (Screen s) = Screen $ map (switchOn a) (take b s) ++ drop b s
  where
    switchOn a r = replicate a True ++ drop a r

 -- rotate row y
rotateRowY :: Int -> Int -> Screen -> Screen
rotateRowY a b (Screen s) =
  Screen $ take a s ++ [rotate b (s !! a)] ++ drop (a + 1) s
  where
    rotate :: Int -> [Bool] -> [Bool]
    rotate b s = take (length s) . drop (length s - b) $ cycle s

 -- rotate column x
rotateColumnX :: Int -> Int -> Screen -> Screen
rotateColumnX a b (Screen s) =
  Screen (transpose . fromScreen . rotateRowY a b $ Screen (transpose s))

 -- Extract numbers from a string
getNumbers :: String -> [Int]
getNumbers x
  | x == "" = []
  | isDigit (head x) =
    (read :: String -> Int) (takeWhile isDigit x) :
    getNumbers (dropWhile isDigit x)
  | otherwise = getNumbers $ dropWhile (not . isDigit) x

 -- Parse a command and call the corresponding transformation
parseCommand :: String -> Screen -> Screen
parseCommand c s
  | "rect" `isPrefixOf` c = rect a b s
  | "row" `isInfixOf` c = rotateRowY a b s
  | "column" `isInfixOf` c = rotateColumnX a b s
  | otherwise = error "Wrong command"
  where
    [a, b] = getNumbers c

 -- Solution to the first star
star1 :: [String] -> Int
star1 = sum . map (fromEnum) . concat . fromScreen . star2

 -- Solution to the second star
star2 :: [String] -> Screen
star2 = foldl (flip parseCommand) (newScreen 50 6)

main = do
  -- s <- readFile "testinput"
  -- print $ lines s
  -- print $ foldl (flip parseCommand) (newScreen 7 3) (lines s)

  s <- readFile "input"
  print $ star1 (lines s)
  print $ star2 (lines s)
