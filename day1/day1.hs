import           Data.Hashable
import           Data.HashSet
import           Data.Maybe

type Direction = Char

type Turn = Char

type Steps = Int

data City =
  City (Int, Int) Direction
  deriving (Show)

instance Hashable City where
  hashWithSalt x (City p _) = hashWithSalt x p

instance Eq City where
  (==) (City p1 _) (City p2 _) = p1 == p2

-- How directions are transformed by turning around
turnTo :: Direction -> Turn -> Direction
turnTo 'N' 'L' = 'W'
turnTo 'N' 'R' = 'E'
turnTo 'E' 'L' = 'N'
turnTo 'E' 'R' = 'S'
turnTo 'S' 'L' = 'E'
turnTo 'S' 'R' = 'W'
turnTo 'W' 'L' = 'S'
turnTo 'W' 'R' = 'N'
turnTo x 'N'   = x

-- A rotation action
rotate :: Turn -> City -> City
rotate r (City p d) = City p (turnTo d r)

-- Move a certain number of steps
move :: Steps -> City -> City
move s (City (x, y) 'N') = City (x, (y + s)) 'N'
move s (City (x, y) 'E') = City ((x + s), y) 'E'
move s (City (x, y) 'S') = City (x, (y - s)) 'S'
move s (City (x, y) 'W') = City ((x - s), y) 'W'

-- Type defining a movement command
data Command =
  Command Turn Steps
  deriving (Eq, Show, Read)

-- Apply the command to a state
apply :: Command -> City -> City
apply (Command t s) = move s . rotate t

-- Apply all commands in a list of commands to a state
applyAll :: City -> [Command] -> City
applyAll = foldl (flip apply)

-- Compute the distance of a position to the initial position (0,0)
distance :: City -> Steps
distance (City (x, y) _) = abs x + abs y

-- Split the input string into List of commands
parseCommands :: String -> [Command]
parseCommands "" = []
parseCommands (' ':xs) = parseCommands xs
parseCommands (',':xs) = parseCommands xs
parseCommands xs = first : parseCommands second
  where
    (x:rest, second) = span (/= ',') xs
    first = Command x ((read :: String -> Int) rest)

-- Solution to first star
star1 :: String -> Int
star1 = distance . (applyAll (City (0, 0) 'N')) . parseCommands

-- Expand commands into a list of elementary commands
expandCommands :: [Command] -> [Command]
expandCommands [] = []
expandCommands ((Command d s):xs) =
  ((Command d 1) : (replicate (s - 1) (Command 'N' 1))) ++ expandCommands xs

-- create Memoized version of applyAll to find the first duplicated element
findDuplicate :: (Eq a, Hashable a) => [a] -> Maybe a
findDuplicate xs = go empty xs
  where
    go _ [] = Nothing
    go seen (x:xs) =
      if x `member` seen
        then Just x
        else go (x `insert` seen) xs

-- Solution to second star
star2 :: String -> Int
star2 =
  distance .
  fromJust .
  findDuplicate .
  (scanl (flip apply) (City (0, 0) 'N')) . expandCommands . parseCommands

-- Simple test cases
test1 = star1 "R2, L3" == 5

test2 = star1 "R2, R2, R2" == 2

test3 = star1 "R5, L5, R5, R3" == 12

test4 = star2 "R8, R4, R4, R8" == 4

main = do
  s <- readFile "input"
  print test1
  print test2
  print test3
  print $ test4
  print $ star1 s
  print $ star2 s
