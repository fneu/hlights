module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Exception (bracket)
import Database.SQLite.Simple
import Dirigera (fetchLights, isConnected)
import Env (Env (..), runApp)
import Layout (layoutRoutes)
import Pages.Connection (connectionRoutes)
import Pages.Debug (debugRoutes)
import Pages.Home (homeRoutes)
import Pages.Schedule (scheduleRoutes)
import Scheduler (startScheduleManager)
import Storage
import Watch (startWatching)
import Web.Scotty.Trans (get, redirect, scottyT)

main :: IO ()
main = do
  let dbFile = "hlights.db"
  bracket (open dbFile) close $ \conn -> do
    initializeDB conn
    lights <- newTVarIO mempty
    connected <- runApp (Env conn lights) isConnected
    if connected
      then do
        fetchedLights <- runApp (Env conn lights) fetchLights
        atomically $ writeTVar lights fetchedLights
      else putStrLn "Not connected to Dirigera, Proceeding without initial lights."
    runApp (Env conn lights) startScheduleManager
    _ <- forkIO $ do
      runApp (Env conn lights) startWatching

    scottyT 3000 (runApp $ Env conn lights) $ do
      get "/" $ redirect "/home"
      layoutRoutes
      connectionRoutes
      homeRoutes
      scheduleRoutes
      debugRoutes
