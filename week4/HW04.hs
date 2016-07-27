{-# OPTIONS_GHC -Wall #-}
module HW04 where

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

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P arr) = 
        let revArr = reverse arr
        in show' revArr
        where show' :: (Num a1, Eq a1, Show a1) => [a1] -> String
              show' [] = ""
              show' [0] = ""
              show' [n] = show (n)
              show' [0, n] = if n /= 0 then show' [n] else ""
              show' [1, n] = "x + " ++ (if n /= 0 then show' [n] else "")
              show' [-1, n] = "-x + " ++ (if n /= 0 then show' [n] else "")
              show' [n1, n2] = show(n1) ++ "x" ++ (if n2 /= 0 then show' [n2] else "")
              show' (0:t) = show' t
              show' (1:t) = "x^" ++ show (length t) ++ " + " ++ show' t
              show' (-1:t) = "-" ++ show' (1:t)
              show' (h:0:t) = (show h) ++ "x^" ++ show (length t + 1) ++ show' t
              show' (h:t) = (show h) ++ "x^" ++ show (length t) ++ " + " ++ show' t
-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (mergeArr a b)
    where mergeArr::(Num a) => [a] -> [a] -> [a]
          mergeArr [] [] = []
          mergeArr arr [] = arr
          mergeArr [] arr = arr
          mergeArr (h1:t1) (h2:t2) = (h1+h2):(mergeArr t1 t2)

-- Exercise 5 -----------------------------------------

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

