module Dirigera (baseURL, ipAddr, authToken, isConnected) where

import Auth (noSSLVerifyManager)
import Control.Exception.Base (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Dirigera.Devices (Device (..))
import Env
import Network.HTTP.Simple
import Storage

baseURL :: AppM Text
baseURL = do
  ip <- Storage.getProperty "DIRIGERA_IP"
  pure $ "https://" <> fromMaybe "homesmart.local" ip <> ":8443/v1"

ipAddr :: AppM Text
ipAddr = do
  ip <- Storage.getProperty "DIRIGERA_IP"
  pure $ fromMaybe "homesmart.local" ip

authToken :: AppM Text
authToken = do
  token <- Storage.getProperty "DIRIGERA_TOKEN"
  pure $ fromMaybe "" token

isConnected :: AppM Bool
isConnected = do
  url <- baseURL
  token <- authToken
  manager <- liftIO noSSLVerifyManager
  request' <- parseRequest ("GET " <> unpack url <> "/hub/status")
  let request =
        setRequestManager manager $
          setRequestHeaders
            [("Authorization", encodeUtf8 $ "Bearer " <> token)]
            request'
  responseResult <- liftIO $ try (httpLBS request) :: AppM (Either HttpException (Response LBS.ByteString))
  case responseResult of
    Left ex -> do
      liftIO $ putStrLn $ "Error checking connection: " <> show ex
      pure False
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body :: Either String Device of
        Left err -> do
          liftIO $ putStrLn $ "Error parsing JSON to Device: " <> err
          pure False
        Right statusResponse -> pure statusResponse.isReachable
