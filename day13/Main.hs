module Main where

import           Control.Monad
import           Data.Char
import qualified Data.Set      as S
import           Numeric

-- | Office designer's favorite number
favoriteNumber :: Int
favoriteNumber = 1362

type Pos = (Int, Int) -- A position in the office

type Path = [Pos] -- A path in the office

type Seen = S.Set Pos -- The set of visited positions in the office

-- | Check if it's a wall according to the poster formula
isWall :: Pos -> Bool
isWall (x, y) =
  odd . length . filter (== '1') $ showIntAtBase 2 intToDigit funny ""
  where
    funny = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber

-- | Check if the position is inside the building
isValid :: Pos -> Bool
isValid (x, y) = x >= 0 && y >= 0

-- | Nondeterministic step: return the set of paths that are one-step extensions
-- | of the current path (together with the visited nodes)
oneStep :: (Path, Seen) -> [(Path, Seen)]
oneStep (p, seen) = do
  let (x, y) = last p
  p' <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  guard $ not $ p' `S.member` seen
  guard $ (isValid p') && not (isWall p')
  return $ (p ++ [p'], S.insert p' seen)

-- | Find the shortest paths from start to goal
findShortest :: Pos -> Pos -> [Path]
findShortest start goal = map fst $ go [([start], S.singleton start)]
  where
    go :: [(Path, Seen)] -> [(Path, Seen)]
    go paths
      | any ((== goal) . last . fst) paths =
        filter ((== goal) . last . fst) paths
      | otherwise = go (paths >>= oneStep)

-- | Solution to the first star
-- | subtract one because the path contains the starting pos
star1 :: IO ()
star1 = do
  putStr "Star 1: "
  putStrLn . show $
    minimum . fmap ((subtract 1) . length) $ findShortest (1, 1) (31, 39)

-- | Returns all paths starting from start up to n steps
allPaths :: Pos -> Int -> [(Path, Seen)]
allPaths start n = go n $ return ([start], S.singleton start)
  where
    go :: Int -> [(Path, Seen)] -> [(Path, Seen)]
    go 0 paths = paths
    go m paths = paths ++ (go (m - 1) (paths >>= oneStep))

star2 :: IO ()
star2 = do
  let visited = fmap snd $ allPaths (1, 1) 50 -- Get the list of visited nodes in each path
  putStr "Star 2: "
  putStrLn . show . S.size $ foldr (S.union) S.empty visited -- Union of all the visited nodes

main :: IO ()
main = do
  star1
  star2
