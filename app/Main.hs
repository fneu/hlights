{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Text (Text)
import Database.SQLite.Simple
import Lucid
import Lucid.Htmx
import Web.Scotty

-- Database initialization
initializeDB :: FilePath -> IO ()
initializeDB dbFile = do
  conn <- open dbFile
  execute_ conn "CREATE TABLE IF NOT EXISTS Counter (id INTEGER PRIMARY KEY, value INTEGER)"
  -- Insert the initial value if it doesn't exist
  execute_ conn "INSERT OR IGNORE INTO Counter (id, value) VALUES (1, 0)"
  close conn

-- Get the current counter value
getCounter :: Connection -> IO Int
getCounter conn = do
  [Only value] <- query_ conn "SELECT value FROM Counter WHERE id = 1"
  pure value

-- Update the counter value
updateCounter :: Connection -> Int -> IO ()
updateCounter conn newValue = do
  execute conn "UPDATE Counter SET value = ? WHERE id = 1" (Only newValue)

main :: IO ()
main = do
  let dbFile = "hlights.db"
  initializeDB dbFile

  scotty 3000 $ do
    -- Home route displaying the counter and buttons
    get "/" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      liftIO $ close conn
      html $
        renderText $ do
          html_ $ do
            head_ $ do
              title_ "Counter"
              link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
              script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
            body_ $ do
              h1_ [id_ "counter"] (toHtml . show $ counter)
              button_
                [ hxGet_ "/increment",
                  hxTarget_ "#counter",
                  hxSwap_ "outerHTML",
                  class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
                ]
                "inc"
              button_
                [ hxGet_ "/decrement",
                  hxTarget_ "#counter",
                  hxSwap_ "outerHTML",
                  class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
                ]
                "dec"

    -- Increment the counter
    get "/increment" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      let newCounter = counter + 1
      liftIO $ updateCounter conn newCounter
      liftIO $ close conn
      html . renderText $
        h1_ [id_ "counter"] (toHtml . show $ newCounter)

    -- Decrement the counter
    get "/decrement" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      let newCounter = counter - 1
      liftIO $ updateCounter conn newCounter
      liftIO $ close conn
      html . renderText $
        h1_ [id_ "counter"] (toHtml . show $ newCounter)
