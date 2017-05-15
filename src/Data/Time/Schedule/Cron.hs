module Data.Time.Schedule.Cron
  (
    cronToIntegers,
    cronToSchedule,
    enumerateTimesFrom,
    parseScheduleEither,
    module Data.Time.Schedule.Cron.Types,
    module Data.Time.Schedule.Cron.Parser
  ) where
import Control.Monad
import Data.Fixed
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Time.LocalTime
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Calendar.WeekDate as Cal

import Data.Time.Schedule
import Data.Time.Schedule.Cron.Types
import Data.Time.Schedule.Cron.Parser (parseEither)

type SchedSpec = ([Integer], [Int], [Int], IntSet, [Int], [Int], [Pico])

type Min = Integer
type Max = Integer
type RangeMinMax = (Min, Max)

enumerateTimes :: (Integer, Int, Int, Int, Int, Pico) -> SchedSpec -> [LocalTime]
enumerateTimes (y, m, d, h, n, s) (ys, ms, ds, dows, hs, ns, ss)
  = do _y <- ys
       guard (_y >= y)
       _m <- ms
       if _y == y then guard (_m >= m) else pure ()
       _d <- ds
       if _y == y && _m == m then guard (_d >= d) else pure ()
       let day = Cal.fromGregorianValid _y _m _d
       guard (maybe False validDOW day)
       _h <- hs
       if _y == y && _m == m && _d == d then guard (_h >= h) else pure ()
       _n <- ns
       if _y == y && _m == m && _d == d && _h == h then guard (_n >= n) else pure ()
       _s <- ss
       if _y == y && _m == m && _d == d && _h == h && _n == n then guard (_s >= s) else pure ()
       maybe [] pure (LocalTime <$> day <*> makeTimeOfDayValid _h _n _s)
  where validDOW day = let (_, _, d') = Cal.toWeekDate day
                       in d' `IntSet.member` dows

enumerateTimesFrom :: LocalTime -> SchedSpec -> [LocalTime]
enumerateTimesFrom = enumerateTimes . totuple
  where totuple loc = let (y, m, d) = Cal.toGregorian $ localDay loc
                          tod = localTimeOfDay loc
                      in (y, m, d, todHour tod, todMin tod, todSec tod)

rangeToIntegers :: RangeMinMax -> Range -> [Integer]
rangeToIntegers (f, t)  All           = [f..t]
rangeToIntegers (f, t) (Range a b)    = [max f a..min t b]
rangeToIntegers minmax (Interval r i) = fmap snd . (filter filtpred) $ zip [0..] (rangeToIntegers minmax r)
  where filtpred (a, _) = a `mod` i == 0

selectionToIntegers :: RangeMinMax -> Selection -> [Integer]
selectionToIntegers minmax (Exp r)   = rangeToIntegers minmax r
selectionToIntegers (f, t) (List as) = filter filtbetween as
  where filtbetween a = f <= a && a <= t

cronToIntegers :: Cron -> SchedSpec
cronToIntegers (Cron n h d m dow y)
  = (sort $ toints (1900, 3000) y,
     sort $ toints (1, 12) m,
     sort $ toints (1, 31) d,
     IntSet.fromList (toints (1, 7) dow),
     sort $ toints (0, 23) h,
     sort $ toints (0, 59) n,
     [0])
  where toints minmax = fmap toint . selectionToIntegers minmax
        toint a = if a < (fromIntegral (minBound :: Int)) || a > (fromIntegral (maxBound :: Int))
                  then error "int out of bounds" -- shouldn't happen
                  else fromInteger a

cronToSchedule :: ZonedTime -> Cron -> Schedule
cronToSchedule startfrom cron = fromList $ enumWithTZ
  where enumWithTZ = fmap (localTimeToUTC tz) $ enumerateTimesFrom loc (cronToIntegers cron)
        tz = zonedTimeZone startfrom
        loc = zonedTimeToLocalTime startfrom

-- parameterised by a function so that the caller can detect a parse
-- error without initialising the schedule with the current time.
-- caller can use `sequence` if needed.
parseScheduleEither :: String -> Either String (ZonedTime -> Schedule)
parseScheduleEither = fmap (flip cronToSchedule) . parseEither
