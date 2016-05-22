{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n 
    | n <= 0 = []
    | otherwise = [lastDigit(n)] ++ toRevDigits(dropLastDigit n) 

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = (x) : (y * 2) : (doubleEveryOther zs) 

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigitsWithinNumber :: Integer -> Integer
sumDigitsWithinNumber n
    | n < 10 = n
    | otherwise = (lastDigit n) + (sumDigitsWithinNumber (dropLastDigit n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs) 
    | x > 9 = (sumDigitsWithinNumber x) + (sumDigits zs)
    | otherwise = x + sumDigits zs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = sumDigits(doubleEveryOther(toRevDigits(n))) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 la lc _ = [(la, lc)]
hanoi n la lc lb = (hanoi (n - 1) la lb lc) ++ [(la, lc)] ++ (hanoi (n-1) lb lc la)  
