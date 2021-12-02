module Main where

main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve []             = 0
solve [x           ] = 0
solve (x1 : x2 : xs) = (if x1 < x2 then 1 else 0) + solve (x2 : xs)
