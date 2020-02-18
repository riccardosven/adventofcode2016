module Main where


import Numeric
import Data.Char
import Data.List
import qualified Data.Set as S
import Control.Monad


favoriteNumber = 1362


type Pos = (Int, Int)

isWall :: Pos -> Bool
isWall (x,y) = odd . length . filter (=='1') $ showIntAtBase 2 intToDigit funny ""
    where
        funny = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber

isValid :: Pos -> Bool
isValid (x,y) = x >= 0 && y >= 0

type Path = [Pos]
type Seen = S.Set Pos

oneStep :: (Path, Seen) -> [(Path, Seen)]
oneStep (p, seen) = do
    let (x,y) = last p
    p' <- [(x+1, y), (x-1, y), (x, y+1), (x,y-1)]
    guard $ not $ p' `S.member` seen
    guard $ (isValid p') && not (isWall p')
    return $ (p ++ [p'], S.insert p' seen)

findShortest :: Pos -> Pos -> [Path]
findShortest start stop = map fst $ go stop [([start], S.singleton start)]
 where
     go :: Pos -> [(Path, Seen)] -> [(Path, Seen)]
     go goal paths
         | any ((==goal) . last . fst) paths = filter ((==goal) . last .fst) paths
         | otherwise = go goal (paths>>=oneStep)


star1 :: IO ()
star1 = do
    print $ fmap ((subtract 1) . length) $ findShortest (1,1) (31,39)

main :: IO ()
main = do
    --let map = [[if isWall (x,y) then '#' else '.' | y<-[0..9] ] | x <-[0..9]]
    --putStrLn $ intercalate "\n" map

    star1
