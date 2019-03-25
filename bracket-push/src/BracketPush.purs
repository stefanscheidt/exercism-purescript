module BracketPush
  ( isPaired
  ) where

import Prelude

import Data.Foldable (elem)
import Data.List (List(..), filter, fromFoldable, head, tail)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)


isPaired :: String -> Boolean
isPaired = eq (Just Nil) <<< parse <<< normalize

normalize :: String -> List Char
normalize = filter isBracket <<< fromFoldable <<< toCharArray

parse :: List Char -> Maybe (List Char)
parse Nil = Just Nil
parse (Cons '(' xs) = parseBrackets ')' xs
parse (Cons '[' xs) = parseBrackets ']' xs
parse (Cons '{' xs) = parseBrackets '}' xs
parse inp@(Cons x xs)
  | elem x [']','}', ')'] = Just inp
  | otherwise             = Nothing

parseBrackets :: Char -> List Char -> Maybe (List Char)
parseBrackets close xs = do
    subtree <- parse xs
    headElem <- head subtree
    if headElem == close
      then tail subtree >>= parse
      else Nothing

isBracket :: Char -> Boolean
isBracket = flip elem brackets
  where
    brackets = toCharArray "(){}[]"