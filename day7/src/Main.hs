module Main where

import           Data.List.Split

main :: IO ()
main = interact $ show . solve . map read . splitOn ","

solve :: [Int] -> Int
solve xs = minimum $ map totalDelta xs
    where totalDelta target = sum $ map (abs . subtract target) xs
