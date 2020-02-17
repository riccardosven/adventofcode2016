module Main where

import           Common        (getNumbers)
import           Control.Monad
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import           Text.Regex

-- | Models a chip transaction (the act of sending a chip either to another bot or to the output)
data Value = ToBot
    { tgt :: Int
    , val :: (Maybe Int)
    }
    | ToOutput
    { tgt :: Int
    , val :: (Maybe Int)
    }
    deriving (Eq, Show)

-- | Models a bot together
data Bot = Bot
    { low   :: Value
    , high  :: Value
    , chips :: [Int]
    }
    deriving (Eq, Show)

-- | A set of bots
type Bots = M.Map Int Bot

-- | Parse a string into a Bot
parseBot :: String -> Maybe (Int, Bot)
parseBot xs =
  case matchRegex
         (mkRegex
            "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)")
         xs of
    Nothing -> Nothing
    (Just match) -> Just (idxme, Bot wholow whohigh [])
      where idxme = read (match !! 0) :: Int
            idxlow = read (match !! 2) :: Int
            idxhigh = read (match !! 4) :: Int
            wholow =
              if (match !! 1) == "bot"
                then ToBot idxlow Nothing
                else ToOutput idxlow Nothing
            whohigh =
              if (match !! 3) == "bot"
                then ToBot idxhigh Nothing
                else ToOutput idxhigh Nothing

-- | Parse a string into a Value
parseValue :: String -> Maybe Value
parseValue xs =
  case take 3 xs of
    "val" ->
      let w = getNumbers xs
       in Just $ ToBot (w !! 1) (Just (w !! 0))
    _ -> Nothing

-- | Process a list of value messages giving chips to the corresponding bots
processValues :: Bots -> [Value] -> Bots
processValues bs [] = bs
processValues bs ((ToOutput _ _):vs) = processValues bs vs
processValues _ ((ToBot _ Nothing):_) = error "WTF?!"
processValues bs ((ToBot i (Just x)):vs) = processValues bs' vs
  where
    bs' = M.adjust (\o -> o {chips = x : chips o}) i bs

-- | Process a single bot appending to the current value messages
processBot :: [Value] -> Bot -> ([Value], Bot)
processBot prev b
  | length xs < 2 = (prev, b)
  | length xs > 2 = error "WTF?!"
  | otherwise = (prev ++ [lval, hval], b')
  where
    xs = chips b
    [xl, xh] = sort xs
    lval = (low b) {val = Just xl}
    hval = (high b) {val = Just xh}
    b' = b {chips = [], low = lval, high = hval}

-- | Process all bots resulting in a new list of value messages
processBots :: Bots -> ([Value], Bots)
processBots bs = M.mapAccum processBot [] bs

-- | Run the process until no further transactions are made
process :: (Bots, [Value]) -> (Bots, [Value])
process (b, []) = (b, [])
process (b, v) = process (b'', v'')
  where
    b' = processValues b v
    (v'', b'') = processBots b'

-- | Solution to the first step
star1 :: Bots -> IO ()
star1 bs = flip forM_ checkSolution $ zip [0 ..] (M.elems bs)
  where
    checkSolution :: (Int, Bot) -> IO ()
    checkSolution (i, b) =
      if val (low b) == Just 17 && val (high b) == Just 61
        then do
          putStrLn $ "Solution: " ++ show i
        else do
          return ()

-- | Solution to the second step
star2 :: Bots -> IO ()
star2 bs = putStrLn $ "Solution: " ++ (show n)
  where
    n = M.foldl checkSolution 1 bs
    checkSolution :: Int -> Bot -> Int
    checkSolution i (Bot (ToOutput 0 (Just x)) _ _) = i * x
    checkSolution i (Bot (ToOutput 1 (Just x)) _ _) = i * x
    checkSolution i (Bot (ToOutput 2 (Just x)) _ _) = i * x
    checkSolution i (Bot _ (ToOutput 0 (Just x)) _) = i * x
    checkSolution i (Bot _ (ToOutput 1 (Just x)) _) = i * x
    checkSolution i (Bot _ (ToOutput 2 (Just x)) _) = i * x
    checkSolution i (Bot _ _ _)                     = i

main :: IO ()
main = do
  s <- readFile "input"
  let robots = M.fromList . catMaybes . map parseBot . lines $ s
  let values = catMaybes . map parseValue . lines $ s
  let (robots', _) = process (robots, values)
  star1 robots'
  star2 robots'
