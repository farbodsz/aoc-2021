module Main where

import qualified Data.HashMap.Strict           as M
import           Data.List.Split

main :: IO ()
main = interact $ show . solve 256 . map read . splitOn ","

solve :: Int -> [Int] -> Int
solve n = sum . M.elems . runNDays n . listFreqs

listFreqs :: [Int] -> M.HashMap Int Int
listFreqs xs = M.fromListWith (+) [ (x, 1) | x <- xs ]

runNDays :: Int -> M.HashMap Int Int -> M.HashMap Int Int
runNDays n = (!! n) . iterate runDay

runDay :: M.HashMap Int Int -> M.HashMap Int Int
runDay xs =
    M.delete (-1)
        . M.insert 8 newFish
        . M.insertWith (+) 6 newFish
        . M.mapKeys (subtract 1)
        $ xs
    where newFish = M.findWithDefault 0 0 xs
