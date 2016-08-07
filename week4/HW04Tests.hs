module HW4Tests where

import HW04
import Testing

ex02Tests :: [Test]
ex02Tests = [ testF2 "== tests" (==) 
              [
                (P [1, 1], P [1, 1], True),
                (P [1, 1], P [1, 1, 0, 0], True),
                (P [1, 1], P [1, 1, 0, 0, 1], False)
              ]
            ]

ex03Tests :: [Test]
ex03Tests = [ 
              testF2 "showTerm tests" showTerm
              [
                  (0, 5, ""),
                  (1, 1, "x"),
                  (1, 2, "x^2"),
                  (-1, 2, "-x^2"),
                  (3, 1, "3x"),
                  (4, 2, "4x^2"),
                  (-4, 2, "-4x^2")
              ],
              testF1 "show tests" show
              [
                  (P [0], "0"),
                  (P [2], "2"),
                  (P [0, 1], "x"),
                  (P [1, 1], "x + 1"),
                  (P [1, 0], "1"),
                  (P [1, 0, 2], "2x^2 + 1"),
                  (P [2, 1, 2], "2x^2 + x + 2"),
                  (P [2, 1, -2], "-2x^2 + x + 2"),
                  (P [0, 0, 3], "3x^2")
              ]
            ]

ex04Tests :: [Test]
ex04Tests = [
              testF1 "Plus tests" show
              [
                  ((plus (P[0]) (P[0])), "0"),
                  ((plus (P[0]) (P[1])), "1"),
                  ((plus (P[1, 1]) (P[1])), "x + 2"),
                  ((plus (P[2, 1, -2]) (P[-2, 0, 1])), "-x^2 + x")
              ]
            ]

allTests :: [Test]
allTests = concat [ex02Tests, ex03Tests, ex04Tests]