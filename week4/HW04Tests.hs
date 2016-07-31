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

allTests :: [Test]
allTests = concat [ex02Tests]