module Storage
  ( initializeDB,
    getProperty,
    updateProperty,
    Schedule (..),
    listSchedulesByLampId,
    listSchedulesByMinute,
    addSchedule,
    deleteSchedule,
    timeOfDayToMinutes,
    minutesToTimeOfDay,
    getCurrentSchedule,
    utcToLocalTime,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (LocalTime (localTimeOfDay), UTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay (..))
import Data.Time.Zones (loadTZFromDB, utcToLocalTimeTZ)
import Database.SQLite.Simple
import Env (AppM, Env (..))

initializeDB :: Connection -> IO ()
initializeDB conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS Properties \
    \ ( key   TEXT PRIMARY KEY \
    \ , value TEXT \
    \ );"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS Schedules \
    \ ( id               INTEGER PRIMARY KEY AUTOINCREMENT \
    \ , lampId           TEXT NOT NULL \
    \ , timeOfDayMinutes INTEGER NOT NULL \
    \ , brightness       INTEGER NOT NULL \
    \ , colorTemperature INTEGER NOT NULL \
    \ , allowBrighten    BOOLEAN NOT NULL \
    \ , allowDarken      BOOLEAN NOT NULL \
    \ );"

getProperty :: Text -> AppM (Maybe Text)
getProperty prop = do
  conn <- asks (.conn)
  results <-
    liftIO $
      query
        conn
        "SELECT value FROM Properties WHERE key = ? LIMIT 1"
        (Only prop)
  case results of
    [Only value] -> pure (Just value)
    _ -> pure Nothing

updateProperty :: Text -> Text -> AppM ()
updateProperty prop newValue = do
  conn <- asks (.conn)
  liftIO $
    execute
      conn
      "INSERT INTO Properties (key, value) \
      \ VALUES (?, ?) \
      \ ON CONFLICT(key) DO UPDATE SET value = ?"
      (prop, newValue, newValue)

--------------------------------------------------------------------------------
-- 2) Schedule data type
--------------------------------------------------------------------------------

-- | A Schedule in Haskell. Notice we store 'timeOfDay' as a real Haskell
--   'TimeOfDay', but in the DB it's an integer (minutes since midnight).
data Schedule = Schedule
  { -- | primary key
    scheduleId :: Int,
    lampId :: Text,
    -- | from Data.Time.LocalTime
    timeOfDay :: TimeOfDay,
    brightness :: Int,
    colorTemperature :: Int,
    allowBrighten :: Bool,
    allowDarken :: Bool
  }
  deriving (Show, Eq)

-- | Convert a 'TimeOfDay' to an integer (0..1439).
timeOfDayToMinutes :: TimeOfDay -> Int
timeOfDayToMinutes tod =
  let h = todHour tod
      m = todMin tod
   in h * 60 + m

-- | Convert an integer (0..1439) to a 'TimeOfDay'.
--   This is a simple approach that sets 'todSec' = 0.
minutesToTimeOfDay :: Int -> TimeOfDay
minutesToTimeOfDay mins =
  let (h, m) = mins `divMod` 60
   in TimeOfDay
        { todHour = h `mod` 24,
          todMin = fromIntegral m,
          todSec = 0
        }

--------------------------------------------------------------------------------
-- 3) FromRow / ToRow instances
--------------------------------------------------------------------------------

-- We can either do a custom 'FromRow' manually,
-- or store an integer in the 'Schedule' and convert separately.
-- Let's do a manual 'FromRow' so the DB sees an Int, but Haskell sees a TimeOfDay.

instance FromRow Schedule where
  fromRow = do
    sId <- field
    lamp <- field
    mins <- field -- This will be an Int
    br <- field
    ct <- field
    ab <- field
    ad <- field
    pure $
      Schedule
        { scheduleId = sId,
          lampId = lamp,
          timeOfDay = minutesToTimeOfDay mins,
          brightness = br,
          colorTemperature = ct,
          allowBrighten = ab,
          allowDarken = ad
        }

-- For inserting / updating, we can define a manual 'ToRow',
-- or just do it inline in the queries. We'll do a manual approach.
instance ToRow Schedule where
  toRow sch =
    toRow
      ( sch.scheduleId,
        sch.lampId,
        timeOfDayToMinutes sch.timeOfDay,
        sch.brightness,
        sch.colorTemperature,
        sch.allowBrighten,
        sch.allowDarken
      )

--------------------------------------------------------------------------------
-- 4) Schedule CRUD
--------------------------------------------------------------------------------

-- In your Storage module, add:
listSchedulesByLampId :: Text -> AppM [Schedule]
listSchedulesByLampId lampId = do
  conn <- asks (.conn)
  liftIO $
    query
      conn
      "SELECT id, lampId, timeOfDayMinutes, brightness, colorTemperature, allowBrighten, allowDarken \
      \ FROM Schedules \
      \ WHERE lampId = ? \
      \ ORDER BY timeOfDayMinutes ASC"
      (Only lampId)

listSchedulesByMinute :: Int -> AppM [Schedule]
listSchedulesByMinute minute = do
  conn <- asks (.conn)
  liftIO $
    query
      conn
      "SELECT id, lampId, timeOfDayMinutes, brightness, colorTemperature, allowBrighten, allowDarken \
      \ FROM Schedules \
      \ WHERE timeOfDayMinutes = ? \
      \ ORDER BY timeOfDayMinutes ASC"
      (Only minute)

getCurrentSchedule :: Text -> AppM (Maybe Schedule)
getCurrentSchedule lampId = do
  conn <- asks (\e -> e.conn)
  currentTime <- liftIO $ getCurrentTime >>= utcToLocalTime
  let minutes = timeOfDayToMinutes currentTime
  result <-
    liftIO $
      query
        conn
        "SELECT id, lampId, timeOfDayMinutes, brightness, colorTemperature, allowBrighten, allowDarken \
        \ FROM Schedules \
        \ WHERE lampId = ? \
        \ AND timeOfDayMinutes <= ? \
        \ ORDER BY timeOfDayMinutes DESC \
        \ LIMIT 1"
        (lampId, minutes)

  case result of
    [schedule] -> return $ Just schedule
    _ -> getLatestSchedule lampId

getLatestSchedule :: Text -> AppM (Maybe Schedule)
getLatestSchedule lampId = do
  conn <- asks (.conn)
  result <-
    liftIO $
      query
        conn
        "SELECT id, lampId, timeOfDayMinutes, brightness, colorTemperature, allowBrighten, allowDarken \
        \ FROM Schedules \
        \ WHERE lampId = ? \
        \ ORDER BY timeOfDayMinutes DESC \
        \ LIMIT 1"
        (Only lampId)

  case result of
    [schedule] -> return $ Just schedule
    _ -> return Nothing

-- You might need a function to convert UTC time to local time for `timeOfDayToMinutes` to work as expected:
utcToLocalTime :: UTCTime -> IO TimeOfDay
-- TODO: add timezone to settings
utcToLocalTime utcTime = do
  tz <- loadTZFromDB "Europe/Berlin"
  pure $ localTimeOfDay $ utcToLocalTimeTZ tz utcTime

-- | Insert a new schedule. Return the row ID (AUTOINCREMENT).
--   We'll let the database assign the 'id', so we only supply the fields
--   that *aren't* the primary key.
addSchedule ::
  -- | lampId
  Text ->
  -- | timeOfDay (Haskell type)
  TimeOfDay ->
  -- | brightness
  Int ->
  -- | colorTemperature
  Int ->
  -- | allowBrighten
  Bool ->
  -- | allowDarken
  Bool ->
  AppM Int64
addSchedule lamp tod br ct ab ad = do
  conn <- asks (.conn)
  liftIO $ do
    execute
      conn
      "INSERT INTO Schedules \
      \ (lampId, timeOfDayMinutes, brightness, colorTemperature, allowBrighten, allowDarken) \
      \ VALUES (?, ?, ?, ?, ?, ?)"
      ( lamp,
        timeOfDayToMinutes tod,
        br,
        ct,
        ab,
        ad
      )
    lastInsertRowId conn

-- | Delete a schedule by its ID.
deleteSchedule :: Int -> AppM ()
deleteSchedule sId = do
  conn <- asks (.conn)
  liftIO $
    execute
      conn
      "DELETE FROM Schedules WHERE id=?"
      (Only sId)
