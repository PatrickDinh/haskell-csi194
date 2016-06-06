{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (c1:s1) (c2:s2)
    | c1 == c2 = 1 + exactMatches s1 s2
    | otherwise = exactMatches s1 s2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\xs -> length xs - 1) (group (sort (concat [colors, code]))) 

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code1 code2 = sum (map (\xs -> minimum xs) (transpose[(countColors code1), (countColors code2)]))

-- Exercise 3 -----------------------------------------
nonExactMatches :: Code -> Code -> Int
nonExactMatches secret guess = (matches secret guess) - (exactMatches secret guess)

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exactMatches secret guess) (nonExactMatches secret guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c e n) code = (exactMatches c code) == e 
                                 && (nonExactMatches c code) == n 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes (Move _ _ _) [] = []
filterCodes (Move c e n) (code:codes)
    | isConsistent (Move c e n) code = code : (filterCodes (Move c e n) codes)
    | otherwise                      = (filterCodes (Move c e n) codes)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
