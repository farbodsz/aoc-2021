module Main where

import           Data.Char                      ( digitToInt )

main :: IO ()
main = interact $ show . solve . lines

solve :: [String] -> Int
solve xs = binToDec gammaRate * binToDec epsilonRate
  where
    gammaRate   = mostCommonBinDigits . map strToBinary $ xs
    epsilonRate = invertBinary gammaRate

-- List of digits (not perfect, but will do for this solution)
type Binary = [Int]

strToBinary :: String -> Binary
strToBinary = map digitToInt

sumBinaries :: [Binary] -> Binary
sumBinaries = foldr1 (zipWith (+))

mostCommonBinDigits :: [Binary] -> Binary
mostCommonBinDigits xs = map mostCommon $ sumBinaries xs
    where mostCommon d = min 1 $ d `div` (length xs `div` 2)

invertBinary :: Binary -> Binary
invertBinary = map (\x -> (x + 1) `mod` 2)

binToDec :: Binary -> Int
binToDec b = go binNum
  where
    binNum = read $ concatMap show b

    go 0 = 0
    go x = 2 * go (x `div` 10) + (x `mod` 10)
