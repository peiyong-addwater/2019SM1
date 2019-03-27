--  File     : publictests.hs
--  Author   : Peter Schachte
--  Purpose  : test cases for Lab1 project

import Assignment1
import HaskellTest

suite =
  TimeLimit 2.0 $
  Suite [
    expect (subst 0 1 [0,1,2,3]) ([1,1,2,3]),
    expect (interleave [1,2,3,4] [11,12,13,14]) ([1,11,2,12,3,13,4,14]),
    expect (unroll 4 "ski") ("skis")
    ]

main :: IO ()
main = do
  testVerbose suite
