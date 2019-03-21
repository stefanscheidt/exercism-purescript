module BracketPush
  ( isPaired
  , normalize
  ) where

import Prelude

import Data.Foldable (elem)
import Data.List (List(..), filter, fromFoldable, head, tail)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)


isPaired :: String -> Boolean
isPaired _ = false

normalize :: String -> List Char
normalize = filter isBracket <<< fromFoldable <<< toCharArray

s :: List Char -> Maybe (List Char)
s (Cons '[' xs) = do
  subtree <- s xs
  headElem <- head subtree
  if headElem == ']'
    then tail subtree
    else Nothing
s _ = Nothing

isBracket :: Char -> Boolean
isBracket = flip elem brackets
  where
    brackets = toCharArray "(){}[]"