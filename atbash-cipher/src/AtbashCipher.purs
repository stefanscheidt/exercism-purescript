module AtbashCipher
  ( decode
  , encode
  ) where

import Prelude

import Data.Array (drop, filter, intercalate, length, reverse, take, zip)
import Data.Char.Unicode (isAlphaNum, toLower)
import Data.Enum (enumFromTo)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)

-- encode

encodeChar :: Char -> Char
encodeChar c = fromMaybe c $ lookup c cipher

encode :: String -> Maybe String
encode = Just <<< fromCharArray <<< intercalate [' '] <<< groupEvery 5 <<< map encodeChar <<< normalize

-- decode

decodeChar :: Char -> Char
decodeChar = encodeChar

decode :: String -> Maybe String
decode = Just <<< fromCharArray <<< map decodeChar <<< normalize

-- helper

cipher :: Map Char Char
cipher = fromFoldable $ zip aToZ (reverse aToZ)
  where 
    aToZ = enumFromTo 'a' 'z'

normalize :: String -> Array Char
normalize = map toLower <<< filter isAlphaNum <<< toCharArray

groupEvery :: forall a. Int -> Array a -> Array (Array a)
groupEvery n as
  | n >= length as = [as]
  | otherwise = [take n as] <> groupEvery n (drop n as)