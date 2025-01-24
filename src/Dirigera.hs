module Dirigera (switchIsOn, baseURL, ipAddr, authToken, isConnected, isReachable, fetchLights) where

import Auth (noSSLVerifyManager)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Exception.Base (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson (eitherDecode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map (Map, fromList)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Dirigera.Devices (Attributes (..), Device (..), DeviceSet (..))
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

isReachable :: AppM Bool
isReachable = do
  url <- baseURL
  manager <- liftIO noSSLVerifyManager
  request' <- parseRequest ("GET " <> unpack url <> "/hub/status")
  let request = setRequestManager manager request'
  responseResult <- liftIO $ try (httpLBS request) :: AppM (Either HttpException (Response LBS.ByteString))
  case responseResult of
    Left _ -> do
      pure False
    Right _ -> do
      pure True

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
    Left _ -> do
      pure False
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body :: Either String Device of
        Left _ -> do
          pure False
        Right statusResponse -> pure statusResponse.isReachable

fetchLights :: AppM (Map Text Device)
fetchLights = do
  url <- baseURL
  token <- authToken
  manager <- liftIO noSSLVerifyManager
  request' <- parseRequest ("GET " <> unpack url <> "/devices")
  let request =
        setRequestManager manager $
          setRequestHeaders
            [("Authorization", encodeUtf8 $ "Bearer " <> token)]
            request'
  responseResult <- liftIO $ try (httpLBS request) :: AppM (Either HttpException (Response LBS.ByteString))
  case responseResult of
    Left _ -> do
      pure mempty
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body :: Either String [Device] of
        Left _ -> do
          pure mempty
        Right devices -> do
          let lights = filter (\d -> d.deviceType == "light") devices
          pure $ fromList $ map (\d -> (d.id, d)) lights

switchIsOn :: DeviceSet -> Bool -> AppM ()
switchIsOn device setOn = do
  url <- baseURL
  token <- authToken
  manager <- liftIO noSSLVerifyManager
  request' <- parseRequest ("PATCH " <> unpack url <> "/devices/set/" <> unpack device.id)

  let request =
        setRequestManager manager
          $ setRequestHeaders
            [ ("Authorization", encodeUtf8 $ "Bearer " <> token),
              ("Content-Type", "application/json")
            ]
          $ setRequestBodyJSON
            [object ["attributes" .= object ["isOn" .= setOn]]]
            request'
  response <- liftIO $ try (httpLBS request) :: AppM (Either HttpException (Response LBS.ByteString))
  case response of
    Left _ -> do
      liftIO $ putStrLn "Failed to switch device (exception)"
      pure ()
    Right r -> do
      let status = getResponseStatusCode r
      case status of
        202 -> do
          liftIO $ putStrLn $ "Switched device " <> unpack device.id <> " to " <> if setOn then "on" else "off"
          liftIO $ print r
          lights <- asks (.lights)
          _ <- liftIO $ atomically $ do
            modifyTVar' lights $ \l -> fromMaybe l $ do
              let affected = filter (\d -> device `elem` d.deviceSet) (M.elems l)
              let switched = map (\d -> d {attributes = d.attributes {isOn = Just setOn}}) affected
              pure $ fromList $ map (\d -> (d.id, d)) switched
          liftIO $ putStrLn "Switched device"
          pure ()
        _ -> do
          liftIO $ putStrLn "Failed to switch device (status code != 200)"
          liftIO $ print r
          pure ()
