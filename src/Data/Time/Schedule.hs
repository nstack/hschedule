module Data.Time.Schedule where
import Data.Bifunctor
import Data.Hashable
import Data.HashPSQ (HashPSQ, minView)
import qualified Data.HashPSQ as HashPSQ
import Data.List (uncons)
import Data.Time.Clock

-- a list unfold
data Schedule = forall r. Schedule (r -> Maybe (UTCTime, r)) r

type ScheduleQ k = HashPSQ k UTCTime Schedule

until :: UTCTime -> Schedule -> Schedule
until tm = Schedule $ \s -> case runSchedule s of
                              Just (x, t) | x <= tm -> Just (x, t)
                              _                     -> Nothing

peekSchedule :: Schedule -> Maybe UTCTime
peekSchedule a = fmap fst $ runSchedule a

runSchedule :: Schedule -> Maybe (UTCTime, Schedule)
runSchedule (Schedule f a) = fmap (fmap $ Schedule f) $ f a

runScheduleUntil :: UTCTime -> Schedule -> ([UTCTime], Maybe Schedule)
runScheduleUntil lt sch = case runSchedule sch of
                            Nothing -> ([], Nothing)
                            Just (x, y) | x <= lt   -> first (x:) $ runScheduleUntil lt y
                                        | otherwise -> ([], Just sch)

seekSchedule :: Schedule -> UTCTime -> Schedule
seekSchedule s t = case runSchedule s of
                     Nothing      -> s
                     Just (x, s') -> if x < t then seekSchedule s' t
                                              else s

seekScheduleNow :: Schedule -> IO Schedule
seekScheduleNow s = seekSchedule s <$> getCurrentTime

fromList :: [UTCTime] -> Schedule
fromList xs = Schedule uncons xs

toList :: Schedule -> [UTCTime]
toList s = case runSchedule s of
             Nothing -> []
             Just (x, r) -> x : toList r

addToScheduleQ :: (Hashable k, Ord k) => k -> Schedule -> ScheduleQ k -> ScheduleQ k
addToScheduleQ key sch = maybe id insert $ peekSchedule sch
  where insert pri = HashPSQ.insert key pri sch

peekScheduleQ :: (Hashable k, Ord k) => ScheduleQ k -> Maybe UTCTime
peekScheduleQ sch = case minView sch of
                      Just (_, p, _, _) -> Just p
                      Nothing           -> Nothing

runScheduleQ :: (Hashable k, Ord k) => UTCTime -> ScheduleQ k -> ([(k, [UTCTime])], ScheduleQ k)
runScheduleQ tm sch = case minView sch of
                        Just (k, p, v, r) | p <= tm -> let (x, y) = runScheduleQ tm r
                                                           (a, b) = runScheduleUntil tm v
                                                           schnew    = case b of
                                                                      Nothing -> y
                                                                      Just r' -> addToScheduleQ k r' y
                                                       in ((k, a):x, schnew)
                        _ -> ([], sch)
