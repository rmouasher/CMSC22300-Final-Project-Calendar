{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List (sortOn)
import Text.Printf (printf)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

-- Flag Parser
getFlag :: String -> [String] -> Maybe String
getFlag _ [] = Nothing
getFlag flag (x:y:rest)
  | x == flag = Just y
  | otherwise = getFlag flag (y:rest)
getFlag _ [_] = Nothing

-- For flags that don't take a value, like --print
hasFlag :: String -> [String] -> Bool
hasFlag _ [] = False
hasFlag flag (x:xs) = (x == flag) || hasFlag flag xs

usage :: String
usage = unlines
  [ "Usage:"
  , "  cabal run calendar-project -- [--dir <DIR>] --add --id <INT> --title <TEXT> --date <YYYY-MM-DD> --start <HH:MM> --end <HH:MM> [--location <TEXT>] [--notes <TEXT>]"
  , "  cabal run calendar-project -- [--dir <DIR>] --delete <EVENT_ID>"
  , "  cabal run calendar-project -- [--dir <DIR>] --print"
  , ""
  , "Examples:"
  , "  cabal run calendar-project -- --dir ./mycal --add --id 2 --title \"Not study\" --date 2026-02-24 --start 09:00 --end 10:00 --location \"Harper\""
  , "  cabal run calendar-project -- --dir ./mycal --delete 2"
  , "  cabal run calendar-project -- --dir ./mycal --print"
  ]


--  Wrapper types (newtypes)


newtype Title    = Title String
  deriving (Eq, Ord, Show, Read)

newtype Location = Location String
  deriving (Eq, Ord, Show, Read)

newtype Notes    = Notes String
  deriving (Eq, Ord, Show, Read)

newtype EventId  = EventId Int
  deriving (Eq, Ord, Show, Read)

-- Date / time / intervals

-- date type to compare
data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving (Eq, Ord, Show, Read)

-- time-of-day type 
data TimeOfDay = TimeOfDay
  { hour   :: Int
  , minute :: Int
  } deriving (Eq, Ord, Show, Read)


data TimeInterval = TimeInterval
  { start :: TimeOfDay
  , end   :: TimeOfDay
  } deriving (Eq, Ord, Show, Read)

-- Reminders (still need to implement)

data Reminder
  = MinutesBefore Int
  | HoursBefore   Int
  | DaysBefore    Int
  deriving (Eq, Ord, Show, Read)


-- repeating events

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Recurrence
  = NoRecurrence
  | Daily
  | Weekly [Weekday]
  | MonthlyByDay Int
  | Until Date Recurrence
  deriving (Eq, Ord, Show, Read)


-- event type

data Event = Event
  { eventId    :: EventId      
  , title      :: Title         
  , date       :: Date             
  , time       :: Maybe TimeInterval    
  , location   :: Maybe Location   
  , reminders  :: Maybe [Reminder]         
  , recurrence :: Maybe Recurrence         
  , notes      :: Maybe Notes        
  } deriving (Eq, Show, Read)


-- calendar type 


newtype Calendar = Calendar
  { events :: [Event]
  } deriving (Eq, Show, Read)



data ConflictReason
  = Overlap TimeInterval   -- two events overlap during this interval
  | SameStartTime
  deriving (Eq, Show)

--  conflict is a pair of events and the reason.
data Conflict = Conflict
  { left :: Event
  , right :: Event
  , why :: ConflictReason
  } deriving (Eq, Show)


data View
  = DayView Date
  | WeekView Date
  deriving (Eq, Show)

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

isValidIntervalMaybe :: Maybe TimeInterval -> Bool
isValidIntervalMaybe Nothing  = True
isValidIntervalMaybe (Just t) = isValidInterval t

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

--validate optional recurrence
isValidRecurrenceMaybe :: Maybe Recurrence -> Bool
isValidRecurrenceMaybe Nothing  = True
isValidRecurrenceMaybe (Just r) = isValidRecurrence r

isValidRecurrence :: Recurrence -> Bool
isValidRecurrence rec =
  case rec of
    NoRecurrence     -> True
    Daily            -> True
    Weekly _         -> True
    MonthlyByDay n   -> n >= 1 && n <= 31
    Until d innerRec -> isValidDate d && isValidRecurrence innerRec

--validate optional reminder
firstInvalidReminderMaybe :: Maybe [Reminder] -> Maybe Reminder
firstInvalidReminderMaybe Nothing   = Nothing
firstInvalidReminderMaybe (Just rs) = firstInvalidReminder rs

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
      else if not (isValidIntervalMaybe (time ev)) then
        Left ("Invalid event time interval: " ++ show (time ev))
      else if not (isValidRecurrenceMaybe (recurrence ev)) then
        Left ("Invalid recurrence rule: " ++ show (recurrence ev))
      else
        case firstInvalidReminderMaybe (reminders ev) of
          Just badR -> Left ("Invalid reminder: " ++ show badR)
          Nothing   -> Right ev


overlaps :: TimeInterval -> TimeInterval -> Bool
overlaps (TimeInterval s1 e1) (TimeInterval s2 e2) =
  toMinutes s1 < toMinutes e2 && toMinutes s2 < toMinutes e1

-- if events overlap, compute the actual overlapping interval.
overlapInterval :: TimeInterval -> TimeInterval -> Maybe TimeInterval
overlapInterval i1@(TimeInterval s1 e1) i2@(TimeInterval s2 e2)
  | overlaps i1 i2 =
      let s = max s1 s2
          e = min e1 e2
      in Just (TimeInterval s e)
  | otherwise = Nothing

-- handle conflicts between events
conflictBetween :: Event -> Event -> Maybe Conflict
conflictBetween a b
  | date a /= date b = Nothing
  | otherwise =
      case (time a, time b) of
        (Just ta, Just tb) ->
          case overlapInterval ta tb of
            Nothing -> Nothing
            Just ov -> Just (Conflict a b (Overlap ov))
        _ -> Nothing

-- conflicts for adding one event into a calendar
conflictsFor :: Calendar -> Event -> [Conflict]
conflictsFor (Calendar es) newEv = go es
  where
    go [] = []
    go (e:rest) =
      case conflictBetween e newEv of
        Nothing -> go rest
        Just c  -> c : go rest

-- all conflicts in a calendar
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


-- Date and Time Parsers
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : splitOn delim cs
  | otherwise =
      case splitOn delim cs of
        []     -> [[c]]
        (x:xs) -> (c : x) : xs

parseDate :: String -> Maybe Date
parseDate s =
  case splitOn '-' s of
    [ys, ms, ds] ->
      case (readMaybe ys :: Maybe Int, readMaybe ms :: Maybe Int, readMaybe ds :: Maybe Int) of
        (Just y, Just m, Just d) -> Just (Date y m d)
        _                        -> Nothing
    _ -> Nothing

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay s =
  case splitOn ':' s of
    [hs, ms] ->
      case (readMaybe hs :: Maybe Int, readMaybe ms :: Maybe Int) of
        (Just h, Just m) -> Just (TimeOfDay h m)
        _                -> Nothing
    _ -> Nothing


-- Event Interactivity - the loop
readRequiredDate :: IO Date
readRequiredDate = do
  s <- prompt "Date (YYYY-MM-DD):"
  case parseDate s of
    Just d  -> pure d
    Nothing -> do
      putStrLn "Invalid date format. Please try again."
      readRequiredDate

readOptionalTime :: String -> IO (Maybe TimeOfDay)
readOptionalTime label = do
  ms <- promptOptional label
  case ms of
    Nothing -> pure Nothing
    Just s ->
      case parseTimeOfDay s of
        Just t  -> pure (Just t)
        Nothing -> do
          putStrLn "Invalid time format. Please use HH:MM."
          readOptionalTime label

readRequiredInt :: String -> IO Int
readRequiredInt label = do
  s <- prompt label
  case readMaybe s :: Maybe Int of
    Just n  -> pure n
    Nothing -> do
      putStrLn "Please enter a valid integer."
      readRequiredInt label

buildEventInteractive :: IO Event
buildEventInteractive = do
  eidNum <- readRequiredInt "Event ID:"
  ttlStr <- prompt "Title:"
  d      <- readRequiredDate
  mStart <- readOptionalTime "Start (HH:MM, optional):"
  mEnd   <- readOptionalTime "End (HH:MM, optional):"
  mLoc   <- promptOptional "Location (optional):"
  mNotes <- promptOptional "Notes (optional):"

  let mInterval =
        case (mStart, mEnd) of
          (Nothing, Nothing) -> Nothing
          (Just s, Just e)   -> Just (TimeInterval s e)
          _                  -> Nothing

  pure Event
    { eventId    = EventId eidNum
    , title      = Title ttlStr
    , date       = d
    , time       = mInterval
    , location   = fmap Location mLoc
    , reminders  = Nothing
    , recurrence = Nothing
    , notes      = fmap Notes mNotes
    }


-- Interactivity Helpers
prompt :: String -> IO String
prompt label = do
  putStr (label ++ " ")
  hFlush stdout
  getLine

promptOptional :: String -> IO (Maybe String)
promptOptional label = do
  putStr (label ++ " ")
  hFlush stdout
  s <- getLine
  if null s then pure Nothing else pure (Just s)

renderCalendar :: Calendar -> String
renderCalendar (Calendar es) =
  case sortEvents es of
    [] -> "Calendar is empty.\n"
    xs ->
      unlines $
        ("Calendar (" ++ show (length xs) ++ " events):")
        : map renderEventLine xs

sortEvents :: [Event] -> [Event]
sortEvents = sortOn (\e -> (date e, maybe (-1) (toMinutes . start) (time e), eventId e))

renderEventLine :: Event -> String
renderEventLine e =
  let Date y m d = date e
      Title ttl = title e
      timeStr =
        case time e of
          Nothing -> "No time"
          Just (TimeInterval s t) -> renderTime s ++ "-" ++ renderTime t
      locStr =
        case location e of
          Nothing -> ""
          Just (Location loc) -> " @ " ++ loc
      noteStr =
        case notes e of
          Nothing -> ""
          Just (Notes note) -> " | " ++ note
  in printf "%04d-%02d-%02d %s  %s%s%s"
            y m d timeStr ttl locStr noteStr

renderTime :: TimeOfDay -> String
renderTime (TimeOfDay h mn) = printf "%02d:%02d" h mn


calendarFileName :: FilePath
calendarFileName = "calendar.txt"


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

saveCalendar :: FilePath -> Calendar -> IO ()
saveCalendar dir cal = do
  createDirectoryIfMissing True dir
  let fp = dir </> calendarFileName
  writeFile fp (show cal)


readEventId :: String -> Maybe EventId
readEventId s = EventId <$> (readMaybe s :: Maybe Int)
-- nihao


menu :: IO String
menu = do
  putStrLn ""
  putStrLn "Calendar Menu"
  putStrLn "1) Add event"
  putStrLn "2) Delete event"
  putStrLn "3) Print calendar"
  putStrLn "4) Quit"
  prompt "Input:"

appLoop :: FilePath -> Calendar -> IO ()
appLoop dir cal = do
  choice <- menu
  case choice of
    "1" -> do
      ev <- buildEventInteractive
      case validateEvent ev of
        Left err -> do
          putStrLn err
          appLoop dir cal
        Right validEv ->
          case addEvent cal validEv of
            Nothing -> do
              putStrLn "Conflict: event NOT added."
              appLoop dir cal
            Just cal' -> do
              saveCalendar dir cal'
              putStrLn "Added."
              appLoop dir cal'

    "2" -> do
      s <- prompt "Event ID to delete:"
      case readEventId s of
        Nothing -> do
          putStrLn "Invalid event ID."
          appLoop dir cal
        Just eid ->
          case deleteEvent eid cal of
            Nothing -> do
              putStrLn "No such event ID."
              appLoop dir cal
            Just cal' -> do
              saveCalendar dir cal'
              putStrLn "Deleted."
              appLoop dir cal'

    "3" -> do
      putStrLn (renderCalendar cal)
      appLoop dir cal

    "4" -> putStrLn "Goodbye."

    _ -> do
      putStrLn "Invalid choice."
      appLoop dir cal

main :: IO ()
main = do
  args <- getArgs
  let dir = maybe "." id (getFlag "--dir" args)
  cal <- loadCalendar dir
  appLoop dir cal