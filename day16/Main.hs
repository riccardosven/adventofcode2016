module Main where

import Data.List.Split

dragon::String -> String
dragon a = a ++ "0" ++ b
    where
        b = map (\x -> if x=='0' then '1' else '0') (reverse a)

checkSum::String -> String
checkSum s = if odd (length w) then w else checkSum w
    where
        w = reduce s
        reduce [] = []
        reduce (x:y:xs)
            | x==y = '1' : reduce xs
            | otherwise = '0' : reduce xs

fillDisk::Int -> String -> String
fillDisk n s
 | length s >= n = take n s
 | otherwise = fillDisk n (dragon s)

star1:: IO()
star1 = do
    let harddisk = fillDisk 272 "10010000000110000"
    putStrLn $ "Solution 1: " ++ (checkSum harddisk)

star2 :: IO()
star2 = do
    let harddisk = fillDisk 35651584 "10010000000110000"
    putStrLn $ "Solution 2: " ++ (checkSum harddisk)

main :: IO ()
main = do
    star1
    star2
