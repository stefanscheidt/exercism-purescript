module AtbashCipher
  ( decode
  , encode
  ) where

import Prelude

import Data.Array (drop, filter, reverse, snoc, take, zip)
import Data.Char.Unicode (isAlphaNum, toLower)
import Data.Enum (enumFromTo)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst)

-- encode

encode :: String -> Maybe String
encode = Just <<< joinWith " " <<< map fromCharArray <<< package 5 <<< map encodeChar <<< normalize

normalize :: String -> Array Char
normalize = map toLower <<< filter isAlphaNum <<< toCharArray

encodeChar :: Char -> Char
encodeChar c = fromMaybe c $ lookup c encodeMap
  where
    aToZ = enumFromTo 'a' 'z'
    encodeMap = fromFoldable $ zip aToZ (reverse aToZ)

package :: forall a. Int -> Array a -> Array (Array a)
package n as = fst $ package' (Tuple [] as)
  where
    package' :: Tuple (Array (Array a)) (Array a) -> Tuple (Array (Array a)) (Array a)
    package' (Tuple bbs []) = Tuple bbs []
    package' (Tuple bbs bs) = package' (Tuple (snoc bbs (take n bs)) (drop n bs))

-- decode

decode :: String -> Maybe String
decode = Just
