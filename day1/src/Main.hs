module Main where

main :: IO ()
main = interact $ show . solve' . map read . words

-- Part 1
solve :: [Int] -> Int
solve (x1 : x2 : xs) = (if x1 < x2 then 1 else 0) + solve (x2 : xs)
solve _              = 0

-- Part 2
solve' :: [Int] -> Int
solve' = solve . map sumWindow . mkWindows
    where sumWindow (x1, x2, x3) = x1 + x2 + x3

mkWindows :: [Int] -> [(Int, Int, Int)]
mkWindows (x1 : x2 : x3 : xs) = (x1, x2, x3) : mkWindows (x2 : x3 : xs)
mkWindows _                   = []
