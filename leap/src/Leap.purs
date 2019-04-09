module Leap where

import Prelude

import Data.Int (rem)
import Type.Data.Boolean (kind Boolean)

isLeapYear :: Int -> Boolean
isLeapYear year
  | year `divisibleBy` 400 = true
  | year `divisibleBy` 100 = false
  | year `divisibleBy`   4 = true
  | otherwise              = false

divisibleBy :: Int -> Int -> Boolean
divisibleBy candidate divisor = rem candidate divisor == 0
