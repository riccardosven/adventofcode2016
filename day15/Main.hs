module Main where

import           Data.List

-- | solve a x = b (mod p)
solveCongruence :: Integral a => a -> a -> a -> a
solveCongruence a b p = (b * (inv p a)) `mod` p
  where
    inv _ 1 = 1
    inv x y = (n * x + 1) `div` y
      where
        n = y - inv y (x `mod` y)

-- | Amount of time needed until the wheels align as required
-- | Arguments : delay -> number of positions -> initial positions
solve :: [Int] -> [Int] -> [Int] -> Int
solve del pos ini = solveCongruence (sum mul) (negate $ sum pha) lcm
  where
    lcm = product pos
    mul = (lcm `div`) <$> pos
    pha = zipWith (*) mul . zipWith (+) del $ ini

star1 :: [Int] -> [Int] -> [Int] -> IO ()
star1 del pos ini = do
  putStr "Solution 1: "
  print $ solve del pos ini
  return ()

star2 :: [Int] -> [Int] -> [Int] -> IO ()
star2 del pos ini = do
  putStr "Solution 2: "
  print $ solve newdel newpos newini
  return ()
  where
    newdel = del ++ [(length del + 1)]
    newpos = pos ++ [11]
    newini = ini ++ [0]

parse :: String -> [Int]
parse s = read <$> (!!) w <$> [1, 3, 11]
  where
    w = words . delete '.' . delete '#' $ s

main :: IO ()
main = do
  s <- readFile "input"
  let [delay, positions, init] = transpose $ parse <$> lines s
  star1 delay positions init
  star2 delay positions init
