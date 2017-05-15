module Data.Time.Schedule.Repeat where
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Schedule

data FreqOptions = Clip | Rollover

minutely :: FreqOptions -> Integer -> UTCTime -> Schedule
minutely _ inc = grepeat (addUTCTime (60 * fromInteger inc))

hourly :: FreqOptions -> Integer -> UTCTime -> Schedule
hourly _ inc = grepeat (addUTCTime (3600 * fromInteger inc))

daily :: FreqOptions -> Integer -> UTCTime -> Schedule
daily _ inc = grepeat (addUTCTime (86400 * fromInteger inc))

monthly :: FreqOptions -> Integer -> UTCTime -> Schedule
monthly opt inc = grepeat add
  where mut = case opt of
                Clip     -> addGregorianMonthsClip
                Rollover -> addGregorianMonthsRollOver
        add = modifyUTCDay (mut inc)

yearly :: FreqOptions -> Integer -> UTCTime -> Schedule
yearly opt inc = grepeat add
  where mut = case opt of
                Clip     -> addGregorianYearsClip
                Rollover -> addGregorianYearsRollOver
        add = modifyUTCDay (mut inc)

grepeat :: (UTCTime -> UTCTime) -> UTCTime -> Schedule
grepeat fun = Schedule $ \t -> Just (t, fun t)
{-# INLINE grepeat #-}

modifyUTCDay :: (Day -> Day) -> UTCTime -> UTCTime
modifyUTCDay f (UTCTime d t) = UTCTime (f d) t
{-# INLINE modifyUTCDay #-}
