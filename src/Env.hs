module Env (Env (..), runApp, AppM) where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Map (Map)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Dirigera.Devices (Device)

data Env = Env
  { conn :: Connection,
    lights :: TVar (Map Text Device),
    lightUpdates :: TChan Text,
    logs :: TChan Text
  }

type AppM = ReaderT Env IO

runApp :: Env -> AppM a -> IO a
runApp env action = runReaderT action env
