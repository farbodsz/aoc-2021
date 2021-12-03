--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
module Main where

main :: IO ()
main = interact $ show . solve' . map (parse . words) . lines

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

data Command = Forward Int | Up Int | Down Int

data Position = Position
    { posX :: Int
    , posY :: Int
    }

instance Semigroup Position where
    (Position x1 y1) <> (Position x2 y2) = Position (x1 + x2) (y1 + y2)

instance Monoid Position where
    mempty = Position 0 0

parse :: [String] -> Command
parse ["forward", x] = Forward $ read x
parse ["down"   , x] = Down $ read x
parse ["up"     , x] = Up $ read x
parse _              = error "Unexpected format"

solve :: [Command] -> Int
solve = (\Position {..} -> posX * posY) . mconcat . map cmdPos

cmdPos :: Command -> Position
cmdPos (Forward x) = Position x 0
cmdPos (Down    x) = Position 0 x
cmdPos (Up      x) = Position 0 (negate x)

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

data Submarine = Submarine
    { subX   :: Int
    , subY   :: Int
    , subAim :: Int
    }

solve' :: [Command] -> Int
solve' = (\Submarine {..} -> subX * subY) . foldl combineSub (Submarine 0 0 0)

combineSub :: Submarine -> Command -> Submarine
combineSub (Submarine x y a) (Forward n) = Submarine (x + n) (y + a * n) a
combineSub (Submarine x y a) (Down    n) = Submarine x y (a + n)
combineSub (Submarine x y a) (Up      n) = Submarine x y (a - n)

--------------------------------------------------------------------------------
