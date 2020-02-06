import           Data.List
import           Data.List.Split

data Triangle =
  Triangle Int Int Int
  deriving (Eq, Show)

-- Check whether a Triangle is possible
isPossible :: Triangle -> Bool
isPossible (Triangle x1 x2 x3)
  | x1 >= x2 + x3 = False
  | x2 >= x1 + x3 = False
  | x3 >= x1 + x2 = False
  | otherwise = True

-- Turns a list of strings into a triangle
parseTriangle :: [String] -> Triangle
parseTriangle s = Triangle x1 x2 x3
  where
    [x1, x2, x3] = map (read :: String -> Int) s

-- Turns a string into a list of triangles
parseTriangles :: String -> [Triangle]
parseTriangles = map parseTriangle . map words . lines

-- Turns a string with transposed rows into a list of triangles
parseTriangles' :: String -> [Triangle]
parseTriangles' =
  map parseTriangle . concat . map transpose . chunksOf 3 . map words . lines

-- Solution
solution :: [Triangle] -> Int
solution = length . filter isPossible

-- Solution to the first star
star1 :: String -> Int
star1 = solution . parseTriangles

-- Solution to the second star
star2 = solution . parseTriangles'

main = do
  print $ isPossible (Triangle 5 10 25)
  print $ isPossible (Triangle 3 4 5)
  s <- readFile "input"
  print $ star1 s
  print $ star2 s
