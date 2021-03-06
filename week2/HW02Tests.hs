-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import HW02
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Orange], 1)
             , ([Purple, Green, Yellow], [Red, Purple, Orange], 0)
             , ([Purple, Green, Yellow, Red], [Purple, Green, Yellow, Red, Orange], 4)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)
             , ([Red, Red, Blue, Blue], [Red, Red, Green, Green], 2)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2),
               ([Red, Blue, Yellow, Orange], [],
               Move [] 0 0),
               ([Red, Blue, Yellow, Orange], [Purple, Green],
               Move [Purple, Green] 0 0)
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "filterCodes test" filterCodes
             [ (Move [Red, Red, Blue, Green] 1 1, [[Red, Blue, Yellow, Purple], [Red, Blue, Red, Purple], [Red, Blue, Purple, Yellow]],
               [[Red, Blue, Yellow, Purple], [Red, Blue, Purple, Yellow]])
             ] 
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "allCodes test" allCodes [ (0, []) ],
             testF1 "countAllCodes test" countAllCodes [ (0, 0), (2, 36) ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = []

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]
