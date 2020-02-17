module Main where

import qualified Data.Map as M

data Registers = Registers
    { a :: Int
    , b :: Int
    , c :: Int
    , d :: Int
    , p :: Int
    }
    | Value Int
    deriving (Eq, Show)

type Program = M.Map Int (Registers->Registers)

parseGet s =
  case s of
    ""      -> a
    ('a':_) -> a
    ('b':_) -> b
    ('c':_) -> c
    ('d':_) -> d
    ('p':_) -> p
    xs      -> const (toInt xs)

parseSet s =
  case s of
    ""      -> \n r -> r {a = n}
    ('a':_) -> \n r -> r {a = n}
    ('b':_) -> \n r -> r {b = n}
    ('c':_) -> \n r -> r {c = n}
    ('d':_) -> \n r -> r {d = n}
    ('p':_) -> \n r -> r {p = n}

toInt :: String -> Int
toInt = (read :: String -> Int) . takeWhile (/= ' ')

step :: Registers -> Registers
step = (+ 1) <$> parseGet "p" >>= parseSet "p"

parse :: String -> Registers -> Registers
parse s =
  case take 3 s of
    "inc" -> ((+ 1) <$> (parseGet (w !! 1)) >>= (parseSet (w !! 1))) . step
    "dec" ->
      ((subtract 1) <$> (parseGet (w !! 1)) >>= (parseSet (w !! 1))) . step
    "cpy" -> (parseGet (w !! 1) >>= (parseSet (w !! 2))) . step
    "jnz" ->
      \r ->
        if (parseGet (w !! 1) r /= 0)
          then r {p = p r + toInt (w !! 2)}
          else step r
  where
    w = words s

run :: Program -> Registers -> IO Registers
run m r = do
  let pc = p r
  if pc >= length m
    then do
      return r
    else do
      let f = m M.! (p r)
      run m (f r)

star1 :: Program -> IO ()
star1 program = do
  let r = Registers 0 0 0 0 0
  r' <- run program r
  print $ "Solution: " ++ show (a r')
  return ()

star2 :: Program -> IO ()
star2 program = do
  let r = Registers 0 0 1 0 0
  r' <- run program r
  print $ "Solution: " ++ show (a r')
  return ()

main :: IO ()
main = do
  s <- readFile "input"
  let program = M.fromList . zip [0..] . map parse . lines $ s
  star1 program
  star2 program
