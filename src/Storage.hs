module Storage (initializeDB, getProperty, updateProperty) where

import Data.Text (Text)
import Database.SQLite.Simple

initializeDB :: Connection -> IO ()
initializeDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS Properties (key TEXT PRIMARY KEY, value TEXT)"

getProperty :: Connection -> Text -> IO (Maybe Text)
getProperty conn prop = do
  results <- query conn "SELECT value FROM Properties WHERE key = ? LIMIT 1" (Only prop)
  case results of
    [Only value] -> pure (Just value)
    _ -> pure Nothing

updateProperty :: Connection -> Text -> Text -> IO ()
updateProperty conn prop newValue = do
  execute
    conn
    "INSERT INTO Properties (key, value) VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value = ?"
    (prop, newValue, newValue)
