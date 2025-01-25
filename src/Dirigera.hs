module Dirigera (switchIsOn, setColorTemperature, setLightLevel, baseURL, ipAddr, authToken, isConnected, isReachable, fetchLights) where

import Auth (noSSLVerifyManager)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Exception.Base (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Aeson (ToJSON, eitherDecode, object, (.=))
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

patchRequest :: (ToJSON a) => Text -> a -> AppM Bool
patchRequest url body = do
  baseUrl <- baseURL
  token <- authToken
  manager <- liftIO noSSLVerifyManager
  request' <- parseRequest ("PATCH " <> unpack baseUrl <> unpack url)
  let request =
        setRequestManager manager
          $ setRequestHeaders
            [ ("Authorization", encodeUtf8 $ "Bearer " <> token),
              ("Content-Type", "application/json")
            ]
          $ setRequestBodyJSON body request'
  response <- liftIO $ try (httpLBS request) :: AppM (Either HttpException (Response LBS.ByteString))
  case response of
    Left ex -> do
      liftIO $ print ex
      pure False
    Right r -> do
      case getResponseStatusCode r of
        200 -> pure True
        202 -> pure True
        _ -> do
          liftIO $ print r
          pure False

switchIsOn :: DeviceSet -> Bool -> AppM ()
switchIsOn device setOn = do
  let url = "/devices/set/" <> device.id
  let body = [object ["attributes" .= object ["isOn" .= setOn]]]
  success <- patchRequest url body
  if success
    then do
      lights <- asks (.lights)
      _ <- liftIO $ atomically $ do
        modifyTVar' lights $ \l -> fromMaybe l $ do
          Just $
            M.map
              ( \d ->
                  if device `elem` d.deviceSet
                    then
                      d {attributes = d.attributes {isOn = Just setOn}}
                    else d
              )
              l
      pure ()
    else do
      pure ()

type Milliseconds = Int

setColorTemperature :: DeviceSet -> Int -> Milliseconds -> AppM ()
setColorTemperature device temp transitionTime = do
  let url = "/devices/set/" <> device.id
  let body = [object ["attributes" .= object ["colorTemperature" .= temp], "transitionTime" .= transitionTime]]
  success <- patchRequest url body
  if success
    then do
      lights <- asks (.lights)
      _ <- liftIO $ atomically $ do
        modifyTVar' lights $ \l -> fromMaybe l $ do
          Just $
            M.map
              ( \d ->
                  if device `elem` d.deviceSet
                    then
                      d {attributes = d.attributes {colorTemperature = Just temp}}
                    else d
              )
              l
      pure ()
    else do
      pure ()

setLightLevel :: DeviceSet -> Int -> Milliseconds -> AppM ()
setLightLevel device level transitionTime = do
  let url = "/devices/set/" <> device.id
  let body = [object ["attributes" .= object ["lightLevel" .= level], "transitionTime" .= transitionTime]]
  success <- patchRequest url body
  if success
    then do
      lights <- asks (.lights)
      _ <- liftIO $ atomically $ do
        modifyTVar' lights $ \l -> fromMaybe l $ do
          Just $
            M.map
              ( \d ->
                  if device `elem` d.deviceSet
                    then
                      d {attributes = d.attributes {lightLevel = Just level}}
                    else d
              )
              l
      pure ()
    else do
      pure ()
