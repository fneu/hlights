import Data.Text.Lazy qualified as TL
import Database.SQLite.Simple
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
        mconcat
          [ "<h1>Counter: ",
            TL.pack (show counter),
            "</h1>",
            "<button onclick=\"window.location.href='/increment'\">Increment</button>",
            "<button onclick=\"window.location.href='/decrement'\">Decrement</button>"
          ]

    -- Increment the counter
    get "/increment" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      liftIO $ updateCounter conn (counter + 1)
      liftIO $ close conn
      redirect "/"

    -- Decrement the counter
    get "/decrement" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      liftIO $ updateCounter conn (counter - 1)
      liftIO $ close conn
      redirect "/"
