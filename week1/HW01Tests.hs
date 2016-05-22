-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits 
             [(1234, [4, 3, 2, 1]), (0, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d


ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([4, 9, 5, 5], [4, 18,5, 10]), ([1, 2, 3, 4], [1, 4, 3, 8]), ([], []), ([1], [1]), ([0, 0], [0, 0]), ([1,2,3],[1,4,3])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [ Test "testSumDigits test" testSumDigits
             [([1], 1), ([1, 2, 3, 4], 10), ([], 0), ([12], 3), ([10, 5, 18, 4], 19), ([123], 6)]
           ]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, b) = luhn n == b

ex5Tests :: [Test]
ex5Tests = [ Test "testLuhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------
testHaNoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHaNoi (n, la, lc, lb, ms) = hanoi n la lc lb == ms

ex6Tests :: [Test]
ex6Tests = [ Test "testHanoi test" testHaNoi
             [(2, "a", "b", "c",  [("a","c"), ("a","b"), ("c","b")]),
              (2, "a", "c", "b",  [("a","b"), ("a","c"), ("b","c")]),
              (3, "a", "c", "b",  [("a","c"), ("a","b"), ("c","b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
