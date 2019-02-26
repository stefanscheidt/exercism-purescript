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
meetup y m Teenth wd = mDay
  where
    teenth = range 13 19
    mDates = map (\it -> canonicalDate y m <$> toEnum it) teenth
    isWeekday wDay = (map weekday) >>> (eq (Just wDay))
    mDay = join $ head $ filter (isWeekday wd) mDates
meetup y m _ wd = Nothing
