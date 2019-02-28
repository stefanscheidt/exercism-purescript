module Meetup
  ( meetup
  , Week(..)
  ) where

import Prelude

import Data.Array (filter, head, range)
import Data.Date (Date, Month, Weekday, Year, canonicalDate, weekday)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))

data Week = First
          | Second
          | Third
          | Fourth
          | Last
          | Teenth

meetup:: Year -> Month -> Week -> Weekday -> Maybe Date
meetup y m mode wd = resultDate
  where
    days = case mode of
      Teenth -> range 13 19
      First  -> range 1 7
      Second -> range 8 14
      Third  -> range 15 21
      Fourth -> range 22 28
      _      -> []
    dates = map (toDate y m) days
    isWeekday wDay = map weekday >>> eq (Just wDay)
    resultDate = join $ head $ filter (isWeekday wd) dates

toDate :: Year -> Month -> Int -> Maybe Date
toDate y m d = canonicalDate y m <$> toEnum d
