module Env (Env (..), runApp, AppM) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple (Connection)

newtype Env = Env {conn :: Connection}

type AppM = ReaderT Env IO

runApp :: Env -> AppM a -> IO a
runApp env action = runReaderT action env
