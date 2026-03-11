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

-------------------------------------------------------------
--  Wrapper types (newtypes)
-------------------------------------------------------------
newtype Title    = Title String
  deriving (Eq, Ord, Show, Read)

newtype Location = Location String
  deriving (Eq, Ord, Show, Read)

newtype Notes    = Notes String
  deriving (Eq, Ord, Show, Read)

newtype EventId  = EventId Int
  deriving (Eq, Ord, Show, Read)

-------------------------------------------------------------
-- Date / time / intervals / definitions
-------------------------------------------------------------
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
  , recurrence :: Maybe Recurrence         
  , notes      :: Maybe Notes        
  } deriving (Eq, Show, Read)


-- calendar type 

newtype Calendar = Calendar
  { events :: [Event]
  } deriving (Eq, Show, Read)

data ConflictReason
  = Overlap TimeInterval   -- two events overlap during this interval
  deriving (Eq, Show)

--  conflict is a pair of events and the reason.
data Conflict = Conflict
  { left :: Event
  , right :: Event
  , why :: ConflictReason
  } deriving (Eq, Show)

data EventOccurrence = EventOccurrence
  { occSourceId :: EventId
  , occTitle    :: Title
  , occDate     :: Date
  , occTime     :: Maybe TimeInterval
  , occLocation :: Maybe Location
  , occNotes    :: Maybe Notes
  } deriving (Eq, Show)

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

isLeapYear :: Int -> Bool
isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

toMinutes :: TimeOfDay -> Int
toMinutes (TimeOfDay h m) = h * 60 + m

nextDate :: Date -> Date
nextDate (Date y m d)
  | d < daysInMonth y m = Date y m (d + 1)
  | m < 12              = Date y (m + 1) 1
  | otherwise           = Date (y + 1) 1 1

prevDate :: Date -> Date
prevDate (Date y m d)
  | d > 1     = Date y m (d - 1)
  | m > 1     = let pm = m - 1 in Date y pm (daysInMonth y pm)
  | otherwise = Date (y - 1) 12 31

addDaysToDate :: Int -> Date -> Date
addDaysToDate n dt
  | n <= 0    = dt
  | otherwise = addDaysToDate (n - 1) (nextDate dt)

dateInRange :: Date -> Date -> Date -> Bool
dateInRange startD endD d = d >= startD && d <= endD

weekdayFromDate :: Date -> Weekday
weekdayFromDate dt =
  case weekdayIndex dt of
    0 -> Mon
    1 -> Tue
    2 -> Wed
    3 -> Thu
    4 -> Fri
    5 -> Sat
    _ -> Sun

weekdayIndex :: Date -> Int
weekdayIndex d = go (Date 1900 1 1) 0
  where
    go cur n
      | cur == d   = n `mod` 7
      | cur < d    = go (nextDate cur) (n + 1)
      | otherwise  = go (prevDate cur) (n - 1)

stripUntil :: Recurrence -> (Maybe Date, Recurrence)
stripUntil rec =
  case rec of
    Until endDate inner ->
      let (mEnd, core) = stripUntil inner
          newEnd =
            case mEnd of
              Nothing -> Just endDate
              Just e  -> Just (min endDate e)
      in (newEnd, core)
    _ -> (Nothing, rec)

occursOnDate :: Event -> Date -> Bool
occursOnDate ev d
  | d < date ev = False
  | otherwise =
      case recurrence ev of
        Nothing -> d == date ev
        Just rec ->
          let (mUntil, core) = stripUntil rec
          in withinUntil mUntil d && matchesCore core
  where
    withinUntil Nothing _ = True
    withinUntil (Just endD) x = x <= endD

    matchesCore NoRecurrence   = d == date ev
    matchesCore Daily          = d >= date ev
    matchesCore (Weekly days') = d >= date ev && weekdayFromDate d `elem` days'
    matchesCore (MonthlyByDay n) = d >= date ev && day d == n
    matchesCore (Until _ inner) = matchesCore inner

eventOccurrenceOn :: Event -> Date -> EventOccurrence
eventOccurrenceOn ev d =
  EventOccurrence
    { occSourceId = eventId ev
    , occTitle    = title ev
    , occDate     = d
    , occTime     = time ev
    , occLocation = location ev
    , occNotes    = notes ev
    }

expandEventInRange :: Event -> Date -> Date -> [EventOccurrence]
expandEventInRange ev startD endD =
  go startD
  where
    go cur
      | cur > endD = []
      | occursOnDate ev cur = eventOccurrenceOn ev cur : go (nextDate cur)
      | otherwise           = go (nextDate cur)

occurrencesInRange :: Calendar -> Date -> Date -> [EventOccurrence]
occurrencesInRange (Calendar es) startD endD =
  sortOn (\o -> (occDate o, maybe (-1) (toMinutes . start) (occTime o), occSourceId o))
    (concatMap (\e -> expandEventInRange e startD endD) es)


addEvent :: Calendar -> Event -> Maybe Calendar
addEvent cal@(Calendar es) e
  | exists (eventId e) es = Nothing
  | null (conflictsFor cal e) = Just (Calendar (e : es))
  | otherwise = Nothing

-------------------------------------------------------------
-- Delete Event Functions
-------------------------------------------------------------
deleteEvent :: EventId -> Calendar -> Maybe Calendar
deleteEvent targetId (Calendar es)
  | exists targetId es = Just (Calendar (remove targetId es))
  | otherwise          = Nothing

deleteWhere :: (Event -> Bool) -> Calendar -> Maybe Calendar
deleteWhere predicate (Calendar es) =
  let kept = filter (not . predicate) es
  in if length kept == length es
       then Nothing
       else Just (Calendar kept)

deleteByTitle :: String -> Calendar -> Maybe Calendar
deleteByTitle ttl =
  deleteWhere (\e ->
    case title e of
      Title s -> s == ttl)

deleteByDate :: Date -> Calendar -> Maybe Calendar
deleteByDate d =
  deleteWhere (\e -> date e == d)

deleteByDateAndTime :: Date -> TimeInterval -> Calendar -> Maybe Calendar
deleteByDateAndTime d interval =
  deleteWhere (\e -> date e == d && time e == Just interval)

deleteInteractive :: Calendar -> IO Calendar
deleteInteractive cal = do
  putStrLn ""
  putStrLn "Delete Options"
  putStrLn "1) Delete by ID"
  putStrLn "2) Delete by exact title"
  putStrLn "3) Delete by date"
  putStrLn "4) Delete by exact date and time"
  putStrLn "5) Return to menu"
  choice <- prompt "Choose a delete option:"

  case choice of
    "1" -> do
      s <- prompt "Event ID to delete:"
      case readEventId s of
        Nothing -> do
          putStrLn "Invalid event ID."
          pure cal
        Just eid ->
          case deleteEvent eid cal of
            Nothing -> do
              putStrLn "No such event ID."
              pure cal
            Just cal' -> pure cal'

    "2" -> do
      ttl <- prompt "Exact title to delete:"
      case deleteByTitle ttl cal of
        Nothing -> do
          putStrLn "No event matched that title."
          pure cal
        Just cal' -> pure cal'

    "3" -> do
      d <- readRequiredDate
      case deleteByDate d cal of
        Nothing -> do
          putStrLn "No event matched that date."
          pure cal
        Just cal' -> pure cal'

    "4" -> do
      d <- readRequiredDate
      mStart <- readOptionalTime "Start (HH:MM):"
      mEnd   <- readOptionalTime "End (HH:MM):"
      case (mStart, mEnd) of
        (Just s, Just e) ->
          case deleteByDateAndTime d (TimeInterval s e) cal of
            Nothing -> do
              putStrLn "No event matched that date and time."
              pure cal
            Just cal' -> pure cal'
        _ -> do
          putStrLn "You must provide both start and end times."
          pure cal

    "5" -> pure cal

    _ -> do
      putStrLn "Invalid delete option."
      deleteInteractive cal


-- update event
updateEvent :: EventId -> Event -> Calendar -> Maybe Calendar
updateEvent targetId newEv (Calendar es)
  | not (exists targetId es) = Nothing
  | eventId newEv /= targetId = Nothing
  | otherwise =
      case validateEvent newEv of
        Left _ -> Nothing
        Right validEv ->
          let calWithoutOld = Calendar (remove targetId es)
          in addEvent calWithoutOld validEv

exists :: EventId -> [Event] -> Bool
exists _ []     = False
exists i (e:es) = eventId e == i || exists i es

findEvent :: EventId -> [Event] -> Maybe Event
findEvent _ [] = Nothing
findEvent eid (e:es)
  | eventId e == eid = Just e
  | otherwise        = findEvent eid es

remove :: EventId -> [Event] -> [Event]
remove _ [] = []
remove i (e:es)
  | eventId e == i = remove i es
  | otherwise      = e : remove i es

--Validation checks
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
        Right ev



promptKeep :: String -> String -> IO String
promptKeep label oldVal = do
  putStr (label ++ " [" ++ oldVal ++ "]: ")
  hFlush stdout
  s <- getLine
  if null s then pure oldVal else pure s

promptOptionalKeep :: String -> Maybe String -> IO (Maybe String)
promptOptionalKeep label oldVal = do
  let shown =
        case oldVal of
          Nothing -> ""
          Just v  -> v
  putStr (label ++ " [" ++ shown ++ "]: ")
  hFlush stdout
  s <- getLine
  if null s
    then pure oldVal
    else pure (Just s)


showDate :: Date -> String
showDate (Date y m d) = printf "%04d-%02d-%02d" y m d

showMaybeTimeInterval :: Maybe TimeInterval -> String
showMaybeTimeInterval Nothing = ""
showMaybeTimeInterval (Just (TimeInterval s e)) =
  renderTime s ++ "-" ++ renderTime e

showMaybeLocation :: Maybe Location -> Maybe String
showMaybeLocation Nothing = Nothing
showMaybeLocation (Just (Location s)) = Just s

showMaybeNotes :: Maybe Notes -> Maybe String
showMaybeNotes Nothing = Nothing
showMaybeNotes (Just (Notes s)) = Just s

renderDayView :: Calendar -> Date -> String
renderDayView cal d =
  renderOccurrencesWithHeader
    ("Day view: " ++ showDate d)
    (occurrencesInRange cal d d)

renderWeekView :: Calendar -> Date -> String
renderWeekView cal startD =
  let endD = addDaysToDate 6 startD
  in renderOccurrencesWithHeader
       ("Week view: " ++ showDate startD ++ " to " ++ showDate endD)
       (occurrencesInRange cal startD endD)

renderMonthView :: Calendar -> Int -> Int -> String
renderMonthView cal y m =
  let startD = Date y m 1
      endD   = Date y m (daysInMonth y m)
  in renderOccurrencesWithHeader
       (printf "Month view: %04d-%02d" y m)
       (occurrencesInRange cal startD endD)

renderYearView :: Calendar -> Int -> String
renderYearView cal y =
  let startD = Date y 1 1
      endD   = Date y 12 31
  in renderOccurrencesWithHeader
       (printf "Year view: %04d" y)
       (occurrencesInRange cal startD endD)

renderOccurrenceLine :: EventOccurrence -> String
renderOccurrenceLine occ =
  let Date y m d = occDate occ
      Title ttl = occTitle occ
      timeStr =
        case occTime occ of
          Nothing -> "No time"
          Just (TimeInterval s t) -> renderTime s ++ "-" ++ renderTime t
      locStr =
        case occLocation occ of
          Nothing -> ""
          Just (Location loc) -> " @ " ++ loc
      noteStr =
        case occNotes occ of
          Nothing -> ""
          Just (Notes note) -> " | " ++ note
  in printf "%04d-%02d-%02d %s  %s%s%s"
            y m d timeStr ttl locStr noteStr

renderOccurrencesWithHeader :: String -> [EventOccurrence] -> String
renderOccurrencesWithHeader header occs =
  case occs of
    [] -> header ++ "\nNo events.\n"
    xs -> unlines (header : map renderOccurrenceLine xs)


buildUpdatedEventInteractive :: Event -> IO Event
buildUpdatedEventInteractive oldEv = do
  let eid = eventId oldEv

  ttlStr <- promptKeep "New title" $
    case title oldEv of
      Title s -> s

  dateStr <- promptKeep "New date (YYYY-MM-DD)" (showDate (date oldEv))
  d <- case parseDate dateStr of
         Just goodDate -> pure goodDate
         Nothing -> do
           putStrLn "Invalid date format. Keeping old date."
           pure (date oldEv)

  startStr <- promptKeep "New start (HH:MM, blank keeps old)" $
    case time oldEv of
      Nothing -> ""
      Just (TimeInterval s _) -> renderTime s

  endStr <- promptKeep "New end (HH:MM, blank keeps old)" $
    case time oldEv of
      Nothing -> ""
      Just (TimeInterval _ e) -> renderTime e

  mLocStr <- promptOptionalKeep "New location" (showMaybeLocation (location oldEv))
  mNotesStr <- promptOptionalKeep "New notes" (showMaybeNotes (notes oldEv))

  let mStart =
        if null startStr
          then case time oldEv of
                 Nothing -> Nothing
                 Just (TimeInterval s _) -> Just s
          else parseTimeOfDay startStr

      mEnd =
        if null endStr
          then case time oldEv of
                 Nothing -> Nothing
                 Just (TimeInterval _ e) -> Just e
          else parseTimeOfDay endStr

      mInterval =
        case (mStart, mEnd) of
          (Nothing, Nothing) -> Nothing
          (Just s, Just e)   -> Just (TimeInterval s e)
          _                  -> time oldEv

  pure Event
    { eventId    = eid
    , title      = Title ttlStr
    , date       = d
    , time       = mInterval
    , location   = fmap Location mLocStr
    , recurrence = recurrence oldEv
    , notes      = fmap Notes mNotesStr
    }



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
occurrenceConflict :: EventOccurrence -> EventOccurrence -> Maybe Conflict
occurrenceConflict oa ob
  | occDate oa /= occDate ob = Nothing
  | otherwise =
      case (occTime oa, occTime ob) of
        (Just ta, Just tb) ->
          case overlapInterval ta tb of
            Nothing -> Nothing
            Just ov ->
              let leftEv =
                    Event
                      { eventId    = occSourceId oa
                      , title      = occTitle oa
                      , date       = occDate oa
                      , time       = occTime oa
                      , location   = occLocation oa
                      , recurrence = Nothing
                      , notes      = occNotes oa
                      }
                  rightEv =
                    Event
                      { eventId    = occSourceId ob
                      , title      = occTitle ob
                      , date       = occDate ob
                      , time       = occTime ob
                      , location   = occLocation ob
                      , recurrence = Nothing
                      , notes      = occNotes ob
                      }
              in Just (Conflict leftEv rightEv (Overlap ov))
        _ -> Nothing

conflictBetween :: Event -> Event -> Maybe Conflict
conflictBetween a b =
  let startD = max (date a) (date b)
      endD   = recurrenceLimit a b
      occsA  = expandEventInRange a startD endD
      occsB  = expandEventInRange b startD endD
  in firstOccurrenceConflict occsA occsB

recurrenceLimit :: Event -> Event -> Date
recurrenceLimit a b = minimumDate (eventEndCap a) (eventEndCap b)

eventEndCap :: Event -> Date
eventEndCap ev =
  case recurrence ev of
    Nothing -> date ev
    Just rec ->
      case stripUntil rec of
        (Just endD, _) -> endD
        (Nothing, _)   -> addDaysToDate 365 (date ev)

minimumDate :: Date -> Date -> Date
minimumDate x y = if x <= y then x else y

firstOccurrenceConflict :: [EventOccurrence] -> [EventOccurrence] -> Maybe Conflict
firstOccurrenceConflict [] _ = Nothing
firstOccurrenceConflict (x:xs) ys =
  case firstConflictWith x ys of
    Just c  -> Just c
    Nothing -> firstOccurrenceConflict xs ys

firstConflictWith :: EventOccurrence -> [EventOccurrence] -> Maybe Conflict
firstConflictWith _ [] = Nothing
firstConflictWith x (y:ys) =
  case occurrenceConflict x y of
    Just c  -> Just c
    Nothing -> firstConflictWith x ys

-- conflicts for adding one event into a calendar
conflictsFor :: Calendar -> Event -> [Conflict]
conflictsFor (Calendar es) newEv = go es
  where
    go [] = []
    go (e:rest) =
      case conflictBetween e newEv of
        Nothing -> go rest
        Just c  -> c : go rest

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

parseWeekday :: String -> Maybe Weekday
parseWeekday s =
  case trim s of
    "Mon" -> Just Mon
    "Tue" -> Just Tue
    "Wed" -> Just Wed
    "Thu" -> Just Thu
    "Fri" -> Just Fri
    "Sat" -> Just Sat
    "Sun" -> Just Sun
    _     -> Nothing

parseWeekdays :: String -> [Weekday]
parseWeekdays s = mapMaybeWeekday (splitOn ',' s)

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

readEventId :: String -> Maybe EventId
readEventId s = EventId <$> (readMaybe s :: Maybe Int)

readWeekdays :: IO [Weekday]
readWeekdays = do
  putStrLn "Enter weekdays separated by commas (Mon,Tue,Wed,Thu,Fri,Sat,Sun):"
  s <- getLine
  pure (parseWeekdays s)

mapMaybeWeekday :: [String] -> [Weekday]
mapMaybeWeekday [] = []
mapMaybeWeekday (x:xs) =
  case parseWeekday x of
    Just w  -> w : mapMaybeWeekday xs
    Nothing -> mapMaybeWeekday xs

readRecurrenceInteractive :: IO (Maybe Recurrence)
readRecurrenceInteractive = do
  putStrLn ""
  putStrLn "Recurrence Options"
  putStrLn "1) None"
  putStrLn "2) Daily"
  putStrLn "3) Weekly"
  putStrLn "4) Monthly by day-of-month"
  choice <- prompt "Choose recurrence:"

  coreRec <- case choice of
    "1" -> pure Nothing
    "2" -> pure (Just Daily)
    "3" -> do
      ds <- readWeekdays
      pure (Just (Weekly ds))
    "4" -> do
      n <- readRequiredInt "Day of month (1-31):"
      pure (Just (MonthlyByDay n))
    _ -> do
      putStrLn "Invalid choice. Using no recurrence."
      pure Nothing

  case coreRec of
    Nothing -> pure Nothing
    Just r -> do
      ans <- promptOptional "Until date (YYYY-MM-DD, optional):"
      case ans of
        Nothing -> pure (Just r)
        Just s ->
          case parseDate s of
            Just d  -> pure (Just (Until d r))
            Nothing -> do
              putStrLn "Invalid until-date. Keeping recurrence without end date."
              pure (Just r)


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
  mRec   <- readRecurrenceInteractive
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
    , recurrence = mRec
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

readMonthNumber :: IO Int
readMonthNumber = do
  m <- readRequiredInt "Month (1-12):"
  if m >= 1 && m <= 12
    then pure m
    else do
      putStrLn "Month must be between 1 and 12."
      readMonthNumber

readYearNumber :: IO Int
readYearNumber = readRequiredInt "Year:"

printCalendarInteractive :: Calendar -> IO ()
printCalendarInteractive cal = do
  putStrLn ""
  putStrLn "Print Options"
  putStrLn "1) Specific day"
  putStrLn "2) Specific week"
  putStrLn "3) Specific month"
  putStrLn "4) Specific year"
  putStrLn "5) Return to menu"
  choice <- prompt "Choose a print range:"

  case choice of
    "1" -> do
      d <- readRequiredDate
      putStrLn (renderDayView cal d)

    "2" -> do
      putStrLn "Enter the starting day of the week."
      d <- readRequiredDate
      putStrLn (renderWeekView cal d)

    "3" -> do
      y <- readYearNumber
      m <- readMonthNumber
      putStrLn (renderMonthView cal y m)

    "4" -> do
      y <- readYearNumber
      putStrLn (renderYearView cal y)
    
    "5" -> pure ()

    _ -> putStrLn "Invalid print option."

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

menu :: IO String
menu = do
  putStrLn ""
  putStrLn "Calendar Menu"
  putStrLn "1) Add event"
  putStrLn "2) Delete event"
  putStrLn "3) Update event"
  putStrLn "4) Print calendar"
  putStrLn "5) Quit"
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
      s <- prompt "Event ID to update:"
      case readEventId s of
        Nothing -> do
          putStrLn "Invalid event ID."
          appLoop dir cal
        Just eid ->
          case findEvent eid (events cal) of
            Nothing -> do
              putStrLn "No such event ID."
              appLoop dir cal
            Just oldEv -> do
              putStrLn "Current event:"
              putStrLn (renderEventLine oldEv)
              newEv <- buildUpdatedEventInteractive oldEv
              case updateEvent eid newEv cal of
                Nothing -> do
                  putStrLn "Update failed. Event may be invalid or may conflict."
                  appLoop dir cal
                Just cal' -> do
                  saveCalendar dir cal'
                  putStrLn "Updated."
                  appLoop dir cal'

    "4" -> do
      printCalendarInteractive cal
      appLoop dir cal


    "5" -> putStrLn "Shutting down calendar."

    _ -> do
      putStrLn "Invalid choice."
      appLoop dir cal

main :: IO ()
main = do
  args <- getArgs
  let dir = maybe "." id (getFlag "--dir" args)
  cal <- loadCalendar dir
  appLoop dir cal