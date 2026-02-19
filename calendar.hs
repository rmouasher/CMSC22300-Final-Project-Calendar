--calendar.hs
--Ryan Michuad and Rakan Mouasher

-- Calendar Data Types

{-# OPTIONS_GHC -Wall #-}

-- A minimal, well-typed starting point for a *persistent* (immutable) calendar.
-- “Persistent” here means: insert/delete returns a *new* Calendar value,
-- leaving the old one unchanged (pure functional data structure).

module CalendarTypes where

--------------------------------------------------------------------------------
-- 1) Small wrapper types (newtypes)
--------------------------------------------------------------------------------

-- Wrapping Strings in newtypes prevents mixing up fields by accident
-- (e.g. passing a Location where a Title is expected).
newtype Title    = Title String
  deriving (Eq, Ord, Show)

newtype Location = Location String
  deriving (Eq, Ord, Show)

newtype Notes    = Notes String
  deriving (Eq, Ord, Show)

-- Events should have stable identifiers so we can delete/edit by ID.
newtype EventId  = EventId Int
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- 2) Date / time / intervals
--------------------------------------------------------------------------------

-- A simple date type you can compare/sort.
-- Later you can replace this with Data.Time.Day if you want.
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving (Eq, Ord, Show)

-- A time-of-day type (24-hour clock).
-- Again, you can later swap to Data.Time.LocalTime.TimeOfDay.
data TimeOfDay = TimeOfDay
  { hour   :: Int
  , minute :: Int
  } deriving (Eq, Ord, Show)

-- A time interval during a day.
-- Use *half-open* intervals [start, end) because it makes overlap logic clean:
-- - Event A ends at 10:00 and Event B starts at 10:00 -> NOT overlapping.
data TimeInterval = TimeInterval
  { start :: TimeOfDay
  , end   :: TimeOfDay
  } deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- 3) Reminders (notifications relative to event time)
--------------------------------------------------------------------------------

-- Represent “remind me N minutes/hours/days before the event”.
data Reminder
  = MinutesBefore Int
  | HoursBefore   Int
  | DaysBefore    Int
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- 4) Recurrence (repeating events)
--------------------------------------------------------------------------------

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Enum, Bounded)

-- A small recurrence system:
-- - NoRecurrence: one-off event
-- - Daily: repeats every day
-- - Weekly [Weekday]: repeats on selected weekdays
-- - MonthlyByDay n: repeats on day-of-month n (e.g. 15th)
-- - Until date rec: cap any recurrence with an end date
data Recurrence
  = NoRecurrence
  | Daily
  | Weekly [Weekday]
  | MonthlyByDay Int
  | Until Date Recurrence
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- 5) The core Event type
--------------------------------------------------------------------------------

-- This is your “custom event type” milestone: title/date/time/location/reminders/etc.
data Event = Event
  { eventId    :: EventId            -- unique identifier for insert/delete/edit
  , title      :: Title              -- event name
  , date       :: Date               -- which day it happens
  , time       :: TimeInterval       -- start/end time on that day
  , location   :: Maybe Location     -- optional (Nothing if not specified)
  , reminders  :: [Reminder]         -- zero or more reminders
  , recurrence :: Recurrence         -- repeating pattern (or NoRecurrence)
  , notes      :: Maybe Notes        -- optional extra text
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- 6) Calendar type (persistent data structure)
--------------------------------------------------------------------------------

-- The simplest persistent calendar is just a list of events.
-- Insert/delete return a *new* list, keeping previous versions alive.
-- If you want faster lookups later, you can reorganize into (Date -> [Event]).
newtype Calendar = Calendar
  { events :: [Event]
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- 7) Conflicts: how to represent and report them
--------------------------------------------------------------------------------

data ConflictReason
  = Overlap TimeInterval   -- two events overlap during this interval
  | SameStartTime          -- optional extra categorization you might want
  deriving (Eq, Show)

-- A conflict is a pair of events + the reason.
data Conflict = Conflict
  { left :: Event
  , right :: Event
  , why :: ConflictReason
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- 8) Views for terminal printing
--------------------------------------------------------------------------------

-- A rendering target: either a day view or a week view.
-- For WeekView Date, you can interpret the Date as “the week containing this date”
-- or “week starting at this date” (you’ll pick one rule in your implementation).
data View
  = DayView Date
  | WeekView Date
  deriving (Eq, Show)