module Meetup
  ( meetup
  , Week(..)
  ) where

import Prelude

import Data.Array (find, range)
import Data.Date (Date, Month, Weekday, Year, canonicalDate, lastDayOfMonth, weekday)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..))

data Week = First
          | Second
          | Third
          | Fourth
          | Last
          | Teenth

meetup:: Year -> Month -> Week -> Weekday -> Maybe Date
meetup y m w wd = resultDate
  where
    candidateDays = case w of
      First  -> range 1 7
      Second -> range 8 14
      Third  -> range 15 21
      Fourth -> range 22 28
      Teenth -> range 13 19
      Last   -> range (lastDay - 6) lastDay
        where
          lastDay = fromEnum (lastDayOfMonth y m)

    toDate d = canonicalDate y m <$> toEnum d
    candidateDates = map toDate candidateDays

    isWeekday wDay = eq (Just wDay) <<< map weekday
    resultDate = join $ find (isWeekday wd) candidateDates
