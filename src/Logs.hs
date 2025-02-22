module Logs where

import Control.Concurrent.STM (atomically, dupTChan, readTChan)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (encode)
import Data.ByteString.Builder (stringUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Env (AppM, Env (..))
import Web.Scotty.Trans

-- | Route that serves log messages as Server-Sent Events (SSE).
logRoutes :: ScottyT AppM ()
logRoutes = do
  get "/logs" $ do
    env <- lift ask
    let textChan = env.logs
    -- Duplicate the log channel so that each client gets its own read pointer.
    clientChan <- liftIO $ atomically $ dupTChan textChan
    setHeader "Content-Type" "text/event-stream"
    setHeader "Cache-Control" "no-cache"
    setHeader "Connection" "keep-alive"
    stream $ \write flush -> forever $ do
      msg <- liftIO $ atomically $ readTChan clientChan
      let encoded = decodeUtf8 $ toStrict $ encode msg
          sseEvent = "data: <p>" <> encoded <> "</p>\n\n"
      write $ stringUtf8 $ unpack sseEvent
      flush
