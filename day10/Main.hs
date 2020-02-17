module Main where

import           Common     (getNumbers)
import           Data.List
import qualified Data.Map   as M
import           Data.Maybe
import           Text.Regex
 -- The output type describes who should receive the token and the value of the token

data Value = ToBot
    { tgt :: Int
    , val :: (Maybe Int)
    }
    | ToOutput
    { tgt :: Int
    , val :: (Maybe Int)
    }
    deriving (Eq, Show)

data Bot = Bot
    { low   :: Value -- who should receive the low value
    , high  :: Value -- who should receive the high value
    , chips :: [Int] -- the chips currently in possession of this robot
    }
    deriving (Eq, Show)

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

parseValue :: String -> Maybe Value
parseValue xs =
  case take 3 xs of
    "val" -> let w = getNumbers xs in Just $ ToBot (w !! 1) (Just (w !! 0))
    _ -> Nothing

processValues :: [(Int, Bot)] -> [Value] -> [(Int, Bot)]
processValues bs [] = bs
processValues bs ((ToOutput _ _):vs) = processValues bs vs
processValues _ ((ToBot _ Nothing):_) = error "WTF?!"
processValues bs ((ToBot i (Just x)):vs) =
  processValues (first ++ [(i, b')] ++ tail second) vs
  where
    (first, second) = span ((/= i) . fst) bs
    b = snd $ head second
    b' = b {chips = x : chips b}

processBot :: (Int, Bot) -> ([Value], (Int, Bot))
processBot (i, b)
  | length xs < 2 = ([], (i, b))
  | length xs > 2 = error "WTF?!"
  | otherwise = ([lval, hval], (i, b'))
  where
    xs = chips b
    [xl, xh] = sort xs
    lval = (low b) {val = Just xl}
    hval = (high b) {val = Just xh}
    b' = b {chips = [], low = lval, high = hval}

processBots :: [(Int, Bot)] -> ([Value], [(Int, Bot)])
processBots bs = (concat vs, bs')
  where
    (vs, bs') = unzip $ map processBot bs

process :: ([(Int, Bot)], [Value]) -> ([(Int, Bot)], [Value])
process (b, []) = (b, [])
process (b, v) = process (b'', v'')
  where
    b' = processValues b v
    (v'', b'') = processBots b'

star1 :: [(Int, Bot)] -> IO ()
star1 [] = return ()
star1 ((i, b):bs) = do
  if val (low b) == Just 17 && val (high b) == Just 61
    then do
      putStr "Solution: "
      putStrLn $ show i
    else do
      star1 bs

star2 :: [(Int, Bot)] -> IO ()
star2 bs = go 1 bs
  where
    go :: Int -> [(Int, Bot)] -> IO ()
    go n [] = putStrLn $ "Solution: " ++ (show n)
    go n ((_, b'):bs') =
      case b' of
        Bot (ToOutput 0 (Just x)) _ _ -> go (n * x) bs'
        Bot (ToOutput 1 (Just x)) _ _ -> go (n * x) bs'
        Bot (ToOutput 2 (Just x)) _ _ -> go (n * x) bs'
        Bot _ (ToOutput 0 (Just x)) _ -> go (n * x) bs'
        Bot _ (ToOutput 1 (Just x)) _ -> go (n * x) bs'
        Bot _ (ToOutput 2 (Just x)) _ -> go (n * x) bs'
        Bot _ _ _                     -> go n bs'

main :: IO ()
main = do
  s <- readFile "input"
  let robots = catMaybes . map parseBot . lines $ s
  let values = catMaybes . map parseValue . lines $ s
  let (robots', _) = process (robots, values)
  star1 robots'
  star2 robots'
