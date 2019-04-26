module Allergies where

import Prelude

import Data.Array (elem, filter)
import Data.Int.Bits (and)

type Allergen = { name :: String, value :: Int }

allergen :: Int -> String -> Allergen
allergen value name = { name: name, value: value }

allergens :: Array Allergen
allergens = 
  [allergen   1 "eggs"
  ,allergen   2 "peanuts"
  ,allergen   4 "shellfish"
  ,allergen   8 "strawberries"
  ,allergen  16 "tomatoes"
  ,allergen  32 "chocolate"
  ,allergen  64 "pollen"
  ,allergen 128 "cats"
  ]

allergicTo :: Int -> String -> Boolean
allergicTo n s = s `elem` list n

list :: Int -> Array String
list n = map getName $ filter isMatch allergens
  where
    isMatch a = n `and` a.value == a.value
    getName a = a.name

