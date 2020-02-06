import Numeric (showHex)
import Data.Char

type Keypad = Int
type Direction = Char

-- Move in the first keypad
move :: Keypad -> Direction -> Keypad
move  x 'L'
    | x == 1 = 1
    | x == 4 = 4
    | x == 7 = 7
    | otherwise = x-1
move  x 'R'
    | x == 3 = 3
    | x == 6 = 6
    | x == 9 = 9
    | otherwise = x+1
move  x 'U'
    | x == 1 = 1
    | x == 2 = 2
    | x == 3 = 3
    | otherwise = x-3
move x 'D'
    | x == 7 = 7
    | x == 8 = 8
    | x == 9 = 9
    | otherwise = x+3

-- Move in the second keypad
move' :: Keypad -> Direction -> Keypad
move' 1 'D' = 3
move' 1 _ = 1
move' 5 'R' = 6
move' 5 _ = 5
move' 9 'L' = 8
move' 9 _ = 9
move' 13 'U' = 11
move' 13 _ = 13
move' x 'L'
    | x == 2 || x == 10 = x
    | otherwise = x - 1
move' x 'U'
    | x == 2 || x == 4  = x
    | x == 3 = 1
    | otherwise = x - 4
move' x 'R'
    | x == 4 || x == 12  = x
    | x == 8 = 9
    | otherwise = x+1
move' x 'D'
    | x == 10 || x == 12  = x
    | x == 11 = 13
    | otherwise = x+4

-- Apply a sequence of directions to a button
apply :: Keypad -> [Direction] -> Keypad
apply = foldl move

-- Apply all sequences to a button
applyAll :: Keypad -> [[Direction]] -> [Keypad]
applyAll  = scanl apply 

-- Solution to the first star
star1 =  concat . map show . tail . applyAll 5 . lines 

-- Solution to the second star
star2 =  map toUpper . concat . map (flip showHex "") . tail . scanl (foldl move') 5 . lines

main = do
    print $ apply 5 "ULL" == 1
    print $ apply 1 "RRDDD" == 9
    print $ star1 "ULL\nRRDDD\nLURDL\nUUUUD" == "1985"
    print $ star2 "ULL\nRRDDD\nLURDL\nUUUUD" == "5DB3"
    s <- readFile "input"
    print $ star1 s
    print $ star2 s