{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P [0]) (P []) = True
    (==) (P []) (P [0]) = True
    (==) (P (h:t)) (P []) = if h == 0 then (==) (P t) (P []) else False
    (==) (P []) (P (h:t)) = if h == 0 then (==) (P []) (P t) else False
    (==) (P (h1:t1)) (P (h2:t2)) = if h1 == h2 then (==) (P t1) (P t2) else False
 
-- Exercise 3 -----------------------------------------

showTerm :: (Num a, Eq a, Show a) => a -> a -> String
showTerm 0 _ = ""
showTerm 1 1 = "x"
showTerm cof 0 = (show cof)
showTerm cof 1 = (show cof) ++ "x"
showTerm 1 deg = "x^" ++ (show deg)
showTerm (-1) deg = "-x^" ++ (show deg)
showTerm cof deg = (show cof) ++ "x^" ++ (show deg)

concatTerms :: String -> String -> String
concatTerms "" term2 = term2
concatTerms term1 "" = term1
concatTerms term1 term2 = term1 ++ " + " ++ term2

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P arr) = show' arr 0
        where show' :: (Num a1, Eq a1, Show a1) => [a1] -> a1 -> String
              show' [] _ = ""
              show' (h:t) index = concatTerms (show' t $index + 1) (showTerm h index)

-- Exercise 4 -----------------------------------------
zipWithSum :: Num a => [a] -> [a] -> [a]
zipWithSum arr1 arr2 = map (\e -> sum e) $ transpose [arr1, arr2]

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (zipWithSum a b)

-- Exercise 5 -----------------------------------------

multiplePolyWithNum :: Num a => Poly a -> a -> Poly a
multiplePolyWithNum (P array) n = P ((map (\i -> i * n)) array) 

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

