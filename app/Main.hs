import Control.Exception (bracket)
import Database.SQLite.Simple
import Layout (layoutRoutes)
import Pages.Counter (counterRoutes)
import Storage
import Web.Scotty

main :: IO ()
main = do
  let dbFile = "hlights.db"
  -- Opens a connection to the database file and closes it after the action
  bracket (open dbFile) close $ \conn -> do
    initializeDB conn

    scotty 3000 $ do
      get "/" $ redirect "/counter"
      layoutRoutes
      counterRoutes conn
