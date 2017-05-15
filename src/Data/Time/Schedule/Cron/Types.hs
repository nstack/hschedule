module Data.Time.Schedule.Cron.Types where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Range
  = All
  | Range Integer Integer
  | Interval Range Integer
  deriving (Eq, Ord, Show)
data Selection = List [Integer] | Exp Range
  deriving (Eq, Ord, Show)

data Cron
  = Cron
  { _cronMinutes    :: Selection
  , _cronHours      :: Selection
  , _cronDays       :: Selection
  , _cronMonths     :: Selection
  , _cronDaysOfWeek :: Selection
  , _cronYears      :: Selection -- 1900 - 3000
  }
  deriving (Eq, Ord, Show)
