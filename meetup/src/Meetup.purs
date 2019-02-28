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
meetup y m Teenth wd = teenthDay
  where
    toDate it = canonicalDate y m <$> toEnum it
    teenth = range 13 19
    teenthDates = map toDate teenth
    isWeekday wDay = map weekday >>> eq (Just wDay)
    teenthDay = join $ head $ filter (isWeekday wd) teenthDates
meetup y m _ wd = Nothing
