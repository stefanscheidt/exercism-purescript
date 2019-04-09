module Leap where

import Prelude

import Data.Int (rem)

isLeapYear :: Int -> Boolean
isLeapYear year
  | rem year 400 == 0 = true
  | rem year 100 == 0 = false
  | rem year   4 == 0 = true
  | otherwise         = false
