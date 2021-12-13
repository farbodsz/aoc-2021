--------------------------------------------------------------------------------

module Main where

import           Control.Monad.State.Lazy

--------------------------------------------------------------------------------

main :: IO ()
main = interact $ show . solve . lines

--------------------------------------------------------------------------------

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = x : xs

peek :: Stack a -> a
peek []       = error "Empty stack"
peek (x : xs) = x

pop :: Stack a -> Stack a
pop []       = error "Empty stack"
pop (x : xs) = xs

emptyStack :: Stack a
emptyStack = []

--------------------------------------------------------------------------------

data Bracket = Curly | Square | Angle | Parenthesis
    deriving Eq

data BracketType = Open | Closed

toBracket :: Char -> (Bracket, BracketType)
toBracket '(' = (Parenthesis, Open)
toBracket ')' = (Parenthesis, Closed)
toBracket '{' = (Curly, Open)
toBracket '}' = (Curly, Closed)
toBracket '[' = (Square, Open)
toBracket ']' = (Square, Closed)
toBracket '<' = (Angle, Open)
toBracket '>' = (Angle, Closed)
toBracket _   = error "Unknown bracket"

score :: Bracket -> Int
score Parenthesis = 3
score Curly       = 1197
score Square      = 57
score Angle       = 25137

--------------------------------------------------------------------------------

solve :: [String] -> Int
solve = sum . map solveLine

solveLine :: [Char] -> Int
solveLine cs = go cs emptyStack
  where
    go :: [Char] -> Stack Bracket -> Int
    go []       _  = 0
    go (c : cs) st = case toBracket c of
        (br, Open  ) -> go cs (push br st)
        (br, Closed) -> if peek st == br then go cs (pop st) else score br

--------------------------------------------------------------------------------
