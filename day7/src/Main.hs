module Main where

import           Data.List.Split

main :: IO ()
main = interact $ show . solve' . map read . splitOn ","

solve :: [Int] -> Int
solve xs = minimum $ map totalDelta range
  where
    range = [minimum xs .. maximum xs]
    totalDelta target = sum $ map (abs . subtract target) xs

solve' :: [Int] -> Int
solve' xs = minimum $ map totalDelta range
  where
    range = [minimum xs .. maximum xs]
    totalDelta target = sum $ map (fuelCost . abs . subtract target) xs
    fuelCost dx = (dx * (dx + 1)) `div` 2
