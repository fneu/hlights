module Storage (initializeDB, getCounter, updateCounter) where

import Database.SQLite.Simple

initializeDB :: Connection -> IO ()
initializeDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS Counter (id INTEGER PRIMARY KEY, value INTEGER)"
  execute_ conn "INSERT OR IGNORE INTO Counter (id, value) VALUES (1, 0)"

getCounter :: Connection -> IO Int
getCounter conn = do
  [Only value] <- query_ conn "SELECT value FROM Counter WHERE id = 1"
  pure value

updateCounter :: Connection -> Int -> IO ()
updateCounter conn newValue = do
  execute conn "UPDATE Counter SET value = ? WHERE id = 1" (Only newValue)
