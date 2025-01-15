module Main where

import Control.Exception (bracket)
import Database.SQLite.Simple
import Env (Env (..), runApp)
import Layout (layoutRoutes)
import Pages.Counter (counterRoutes)
import Storage
import Web.Scotty.Trans (get, redirect, scottyT)

main :: IO ()
main = do
  let dbFile = "hlights.db"
  bracket (open dbFile) close $ \conn -> do
    initializeDB conn
    scottyT 3000 (runApp $ Env conn) $ do
      get "/" $ redirect "/counter"
      layoutRoutes
      counterRoutes
