module AtbashCipher
  ( decode
  , encode
  ) where

import Prelude

import Data.Array (drop, filter, reverse, snoc, take, zip)
import Data.Char.Unicode (isAlphaNum, toLower)
import Data.Enum (enumFromTo)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst)

-- encode

encodeChar :: Char -> Char
encodeChar c = fromMaybe c $ lookup c cipher

encode :: String -> Maybe String
encode = Just <<< joinWith " " <<< map fromCharArray <<< package 5 <<< map encodeChar <<< normalize

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

package :: forall a. Int -> Array a -> Array (Array a)
package n as = fst $ package' (Tuple [] as)
  where
    package' (Tuple bbs []) = Tuple bbs []
    package' (Tuple bbs bs) = package' (Tuple (snoc bbs (take n bs)) (drop n bs))
