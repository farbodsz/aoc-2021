module Main where

import           Data.List.Split

main :: IO ()
main = interact $ show . solve 256 . map read . splitOn ","

solve :: Int -> [Int] -> Int
solve n = length . runNDays n

runNDays :: Int -> [Int] -> [Int]
runNDays n = (!! n) . iterate runDay

runDay :: [Int] -> [Int]
runDay [] = []
runDay (x : xs) | x == 0    = 6 : 8 : runDay xs
                | otherwise = x - 1 : runDay xs
