{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List (sortOn)
import Text.Printf (printf)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

-- Very small flag parser: expects --calendar <VALUE> --event <VALUE>
getFlag :: String -> [String] -> Maybe String
getFlag _ [] = Nothing
getFlag flag (x:y:rest)
  | x == flag = Just y
  | otherwise = getFlag flag (y:rest)
getFlag _ [_] = Nothing

usage :: String
usage = unlines
  [ "Usage:"
  , "  cabal run calendar-project -- [--dir <DIR>] --add '<EVENT>'"
  , "  cabal run calendar-project -- [--dir <DIR>] --delete <EVENT_ID>"
  , ""
  , "Notes:"
  , "  - Calendar is stored at <DIR>/calendar.txt (default DIR is current directory '.')"
  , "  - EVENT must be valid Haskell syntax for your Event type (Read instance)"
  ]


--------------------------------------------------------------------------------
-- Small wrapper types (newtypes)
--------------------------------------------------------------------------------

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
-- Date / time / intervals
--------------------------------------------------------------------------------

-- A simple date type to compare/sort.
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving (Eq, Ord, Show, Read)

-- A time-of-day type (24-hour clock).
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
-- Reminders (notifications relative to event time)
--------------------------------------------------------------------------------

-- Represent “remind me N minutes/hours/days before the event”.
data Reminder
  = MinutesBefore Int
  | HoursBefore   Int
  | DaysBefore    Int
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- Recurrence (repeating events)
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
-- Event type
--------------------------------------------------------------------------------

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
-- Calendar type (persistent data structure)
--------------------------------------------------------------------------------

-- The simplest persistent calendar is just a list of events.
-- Insert/delete return a *new* list, keeping previous versions alive.
newtype Calendar = Calendar
  { events :: [Event]
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Conflicts: how to represent and report them
--------------------------------------------------------------------------------

data ConflictReason
  = Overlap TimeInterval   -- two events overlap during this interval
  | SameStartTime
  deriving (Eq, Show)

-- A conflict is a pair of events + the reason.
data Conflict = Conflict
  { left :: Event
  , right :: Event
  , why :: ConflictReason
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Views for terminal printing
--------------------------------------------------------------------------------

-- A rendering target: either a day view or a week view.
-- For WeekView Date, can interpret the Date as “the week containing this date”
-- or “week starting at this date” (will pick a rule soon)
data View
  = DayView Date
  | WeekView Date
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Invariant Helpers
--------------------------------------------------------------------------------
isLeapYear :: Int -> Bool
isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

daysInMonth :: Int -> Int -> Int
daysInMonth y m =
  case m of
    1  -> 31
    2  -> if isLeapYear y then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _  -> 0

toMinutes :: TimeOfDay -> Int
toMinutes (TimeOfDay h m) = h * 60 + m
--------------------------------------------------------------------------------
-- PROGRAM LOGIC
--------------------------------------------------------------------------------
-- Add to Calendar Function & Check for time conflict

addEvent :: Calendar -> Event -> Maybe Calendar
addEvent cal@(Calendar es) e =
  if null (conflictsFor cal e)
     then Just (Calendar (e : es))
     else Nothing

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

-- Basic Invariants and Validation
isValidDate :: Date -> Bool
isValidDate (Date y m d)
  | m < 1 || m > 12   = False
  | d < 1             = False
  | d > daysInMonth y m  = False
  | otherwise = True

isValidTimeOfDay :: TimeOfDay -> Bool
isValidTimeOfDay (TimeOfDay h m)
  | h < 0 || h > 23 = False
  | m < 0 || m > 59 = False
  | otherwise       = True

isValidInterval :: TimeInterval -> Bool
isValidInterval (TimeInterval s e)
  | not (isValidTimeOfDay s) = False
  | not (isValidTimeOfDay e) = False
  | toMinutes s >= toMinutes e = False
  | otherwise = True

validateReminder :: Reminder -> Bool
validateReminder r =
  case r of
    MinutesBefore n -> n >= 0
    HoursBefore   n -> n >= 0
    DaysBefore    n -> n >= 0

isValidRecurrence :: Recurrence -> Bool
isValidRecurrence rec =
  case rec of
    NoRecurrence     -> True
    Daily            -> True
    Weekly _         -> True
    MonthlyByDay n   -> n >= 1 && n <= 31
    Until d innerRec -> isValidDate d && isValidRecurrence innerRec

firstInvalidReminder :: [Reminder] -> Maybe Reminder
firstInvalidReminder [] = Nothing
firstInvalidReminder (r:rs)
  | validateReminder r = firstInvalidReminder rs
  | otherwise          = Just r

validateEvent :: Event -> Either String Event
validateEvent ev =
  case title ev of
    Title "" -> Left "Invalid event: title cannot be empty."
    _ ->
      if not (isValidDate (date ev)) then
        Left ("Invalid event date: " ++ show (date ev))
      else if not (isValidInterval (time ev)) then
        Left ("Invalid event time interval: " ++ show (time ev))
      else if not (isValidRecurrence (recurrence ev)) then
        Left ("Invalid recurrence rule: " ++ show (recurrence ev))
      else
        case firstInvalidReminder (reminders ev) of
          Just badR -> Left ("Invalid reminder: " ++ show badR)
          Nothing   -> Right ev

--------------------------------------------------------------------------------
-- Conflict Handling
--------------------------------------------------------------------------------
-- Check for intv time overlap
overlaps :: TimeInterval -> TimeInterval -> Bool
overlaps (TimeInterval s1 e1) (TimeInterval s2 e2) =
  toMinutes s1 < toMinutes e2 && toMinutes s2 < toMinutes e1

-- If events overlap, compute the actual overlapping interval.
overlapInterval :: TimeInterval -> TimeInterval -> Maybe TimeInterval
overlapInterval i1@(TimeInterval s1 e1) i2@(TimeInterval s2 e2)
  | overlaps i1 i2 =
      let s = max s1 s2
          e = min e1 e2
      in Just (TimeInterval s e)
  | otherwise = Nothing

-- Handles conflicts between events
conflictBetween :: Event -> Event -> Maybe Conflict
conflictBetween a b
  | date a /= date b = Nothing
  | otherwise =
      case overlapInterval (time a) (time b) of
        Nothing -> Nothing
        Just ov -> Just (Conflict a b (Overlap ov))

-- Conflicts for adding one event into a calendar
conflictsFor :: Calendar -> Event -> [Conflict]
conflictsFor (Calendar es) newEv = go es
  where
    go [] = []
    go (e:rest) =
      case conflictBetween e newEv of
        Nothing -> go rest
        Just c  -> c : go rest

-- All conflicts in a calendar
allConflicts :: Calendar -> [Conflict]
allConflicts (Calendar es) = go es
  where
    go [] = []
    go (e:rest) = conflictsWith e rest ++ go rest

    conflictsWith :: Event -> [Event] -> [Conflict]
    conflictsWith _ [] = []
    conflictsWith e (x:xs) =
      case conflictBetween e x of
        Nothing -> conflictsWith e xs
        Just c  -> c : conflictsWith e xs


--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------

-- Render the whole calendar: header + events line-by-line
renderCalendar :: Calendar -> String
renderCalendar (Calendar es) =
  case sortEvents es of
    [] -> "Calendar is empty.\n"
    xs ->
      unlines $
        ("Calendar (" ++ show (length xs) ++ " events):")
        : map renderEventLine xs

-- Sort by (date, start time) so printing is chronological
sortEvents :: [Event] -> [Event]
sortEvents = sortOn (\e -> (date e, start (time e), eventId e))

-- One-line pretty print for an event
renderEventLine :: Event -> String
renderEventLine e =
  let Date y m d = date e
      TimeInterval s t = time e
      Title ttl = title e
      locStr =
        case location e of
          Nothing -> ""
          Just (Location loc) -> " @ " ++ loc
      noteStr =
        case notes e of
          Nothing -> ""
          Just (Notes note) -> " @ " ++ note
  in printf "%04d-%02d-%02d %s-%s  %s%s%s"
            y m d (renderTime s) (renderTime t) ttl locStr noteStr

renderTime :: TimeOfDay -> String
renderTime (TimeOfDay h mn) = printf "%02d:%02d" h mn


-- Where we store the calendar inside the directory
calendarFileName :: FilePath
calendarFileName = "calendar.txt"

-- Load calendar from dir/calendar.txt if it exists and parses;
-- otherwise return an empty calendar.
loadCalendar :: FilePath -> IO Calendar
loadCalendar dir = do
  let fp = dir </> calendarFileName
  existsFile <- doesFileExist fp
  if not existsFile
    then pure (Calendar [])
    else do
      s <- readFile fp
      case readMaybe s :: Maybe Calendar of
        Just cal -> pure cal
        Nothing  -> pure (Calendar [])

-- Save calendar to dir/calendar.txt (creates dir if needed).
saveCalendar :: FilePath -> Calendar -> IO ()
saveCalendar dir cal = do
  createDirectoryIfMissing True dir
  let fp = dir </> calendarFileName
  writeFile fp (show cal)


readEventId :: String -> Maybe EventId
readEventId s = EventId <$> (readMaybe s :: Maybe Int)

main :: IO ()
main = do
  args <- getArgs

  let dir = maybe "." id (getFlag "--dir" args)
      mAddStr = getFlag "--add" args
      mDelStr = getFlag "--delete" args

  -- Load calendar (or empty)
  cal <- loadCalendar dir

  case (mAddStr, mDelStr) of
    (Just _, Just _) ->
      putStrLn "Error: choose only one of --add or --delete.\n" >> putStrLn usage

    (Just evStr, Nothing) ->
      case readMaybe evStr :: Maybe Event of
        Nothing -> putStrLn ("Could not parse --add EVENT.\n\n" ++ usage)
        Just ev ->
          case validateEvent ev of
            Left err -> putStrLn err
            Right validEv ->
              case addEvent cal validEv of
                Nothing -> putStrLn "Conflict: event NOT added."
                Just cal' -> do
                  saveCalendar dir cal'
                  putStrLn "Added. Current calendar:"
                  putStrLn (renderCalendar cal')

    (Nothing, Just delStr) ->
      case readEventId delStr of
        Nothing -> putStrLn ("Could not parse --delete EVENT_ID (expected an Int).\n\n" ++ usage)
        Just eid ->
          case deleteEvent eid cal of
            Nothing -> putStrLn "No such event ID; nothing deleted."
            Just cal' -> do
              saveCalendar dir cal'
              putStrLn "Deleted. Current calendar:"
              putStrLn (renderCalendar cal')

    (Nothing, Nothing) ->
      putStrLn usage