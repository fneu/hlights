module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Exception (bracket)
import Database.SQLite.Simple
import Dirigera (fetchLights, isConnected)
import Env (Env (..), runApp)
import Layout (layoutRoutes)
import Logs (logRoutes)
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
    updateChan <- newTChanIO
    logChan <- newTChanIO
    let env = Env conn lights updateChan logChan
    connected <- runApp env isConnected
    if connected
      then do
        fetchedLights <- runApp env fetchLights
        atomically $ writeTVar lights fetchedLights
      else putStrLn "Not connected to Dirigera, Proceeding without initial lights."
    runApp env startScheduleManager
    _ <- forkIO $ do
      runApp env startWatching

    scottyT 3000 (runApp env) $ do
      get "/" $ redirect "/home"
      layoutRoutes
      connectionRoutes
      homeRoutes
      scheduleRoutes
      debugRoutes
      logRoutes
