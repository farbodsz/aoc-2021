module Main where

main :: IO ()
main = interact $ show . solve . map words . lines

-- Part 1
-- Format: [[forward, 5], [down, 8], ...]
solve :: [[String]] -> Int
solve = uncurry (*) . foldr (sumTuple . parseCommand) (0, 0)
    where sumTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Returns (x, y) from command text
parseCommand :: [String] -> (Int, Int)
parseCommand ["forward", x] = (read x, 0)
parseCommand ["down"   , x] = (0, read x)
parseCommand ["up"     , x] = (0, negate (read x))
parseCommand _              = error "Unexpected format"
