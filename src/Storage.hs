module Storage (initializeDB, getProperty, updateProperty) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text)
import Database.SQLite.Simple
import Env (AppM, Env (..))

initializeDB :: Connection -> IO ()
initializeDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS Properties (key TEXT PRIMARY KEY, value TEXT)"

getProperty :: Text -> AppM (Maybe Text)
getProperty prop = do
  conn <- asks (.conn)
  results <- liftIO $ query conn "SELECT value FROM Properties WHERE key = ? LIMIT 1" (Only prop)
  case results of
    [Only value] -> pure (Just value)
    _ -> pure Nothing

updateProperty :: Text -> Text -> AppM ()
updateProperty prop newValue = do
  conn <- asks (.conn)
  liftIO $
    execute
      conn
      "INSERT INTO Properties (key, value) VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value = ?"
      (prop, newValue, newValue)
