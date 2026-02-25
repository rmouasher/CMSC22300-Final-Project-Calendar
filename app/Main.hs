{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Very small flag parser: expects --calendar <VALUE> --event <VALUE>
getFlag :: String -> [String] -> Maybe String
getFlag _ [] = Nothing
getFlag flag (x:y:rest)
  | x == flag = Just y
  | otherwise = getFlag flag (y:rest)
getFlag _ [_] = Nothing

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  runghc Main.hs --calendar '<CALENDAR>' --event '<EVENT>'"
    , ""
    , "Example:"
    , "  runghc Main.hs \\"
    , "    --calendar \"Calendar {events = []}\" \\"
    , "    --event \"Event { eventId = EventId 1, title = Title \\\"Study\\\", date = Date 2026 2 24,"
    , "                  time = TimeInterval (TimeOfDay 9 0) (TimeOfDay 10 0), location = Nothing,"
    , "                  reminders = [], recurrence = NoRecurrence, notes = Nothing }\""
    ]


--------------------------------------------------------------------------------
-- 1) Small wrapper types (newtypes)
--------------------------------------------------------------------------------

-- Wrapping Strings in newtypes prevents mixing up fields by accident
-- (e.g. passing a Location where a Title is expected).
newtype Title    = Title String
  deriving (Eq, Ord, Show, Read)

newtype Location = Location String
  deriving (Eq, Ord, Show, Read)

newtype Notes    = Notes String
  deriving (Eq, Ord, Show, Read)

-- Events should have stable identifiers so we can delete/edit by ID.
newtype EventId  = EventId Int
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- 2) Date / time / intervals
--------------------------------------------------------------------------------

-- A simple date type you can compare/sort.
-- Later you can replace this with Data.Time.Day if you want.
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving (Eq, Ord, Show, Read)

-- A time-of-day type (24-hour clock).
-- Again, you can later swap to Data.Time.LocalTime.TimeOfDay.
data TimeOfDay = TimeOfDay
  { hour   :: Int
  , minute :: Int
  } deriving (Eq, Ord, Show, Read)

-- A time interval during a day.
-- Use *half-open* intervals [start, end) because it makes overlap logic clean:
-- - Event A ends at 10:00 and Event B starts at 10:00 -> NOT overlapping.
data TimeInterval = TimeInterval
  { start :: TimeOfDay
  , end   :: TimeOfDay
  } deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- 3) Reminders (notifications relative to event time)
--------------------------------------------------------------------------------

-- Represent “remind me N minutes/hours/days before the event”.
data Reminder
  = MinutesBefore Int
  | HoursBefore   Int
  | DaysBefore    Int
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- 4) Recurrence (repeating events)
--------------------------------------------------------------------------------

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Read)

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
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- 6) Calendar type (persistent data structure)
--------------------------------------------------------------------------------

-- The simplest persistent calendar is just a list of events.
-- Insert/delete return a *new* list, keeping previous versions alive.
-- If you want faster lookups later, you can reorganize into (Date -> [Event]).
newtype Calendar = Calendar
  { events :: [Event]
  } deriving (Eq, Show, Read)

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

-- Add to Calendar Function

addEvent :: Calendar -> Event -> Maybe Calendar
addEvent (Calendar es) e = Just (Calendar (e : es))

-- Delete Calendar Function

deleteEvent :: EventId -> Calendar -> Maybe Calendar
deleteEvent targetId (Calendar es)
  | exists targetId es = Just (Calendar (remove targetId es))
  | otherwise          = Nothing

exists :: EventId -> [Event] -> Bool
exists _ []     = False
exists i (e:es) = eventId e == i || exists i es

remove :: EventId -> [Event] -> [Event]
remove _ [] = []
remove i (e:es)
  | eventId e == i = remove i es
  | otherwise      = e : remove i es

main :: IO ()
main = do
  args <- getArgs
  case (getFlag "--calendar" args, getFlag "--event" args) of
    (Just calStr, Just evStr) -> do
      case (readMaybe calStr :: Maybe Calendar, readMaybe evStr :: Maybe Event) of
        (Just cal, Just ev) ->
          case addEvent cal ev of
            Nothing   -> putStrLn "Conflict: event NOT added."
            Just cal' -> do
              putStrLn "Success: event added. New calendar:"
              print cal'
        _ -> putStrLn ("Could not parse --calendar or --event.\n\n" ++ usage)
    _ -> putStrLn usage