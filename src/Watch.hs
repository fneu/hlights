module Watch (connectIgnoringCert) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (mk)
import Data.Default (def)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (TimeOfDay (..))
import Dirigera (authToken, ipAddr, setColorTemperature, setLightLevel)
import Dirigera.Devices (Attributes (..), Device (..), DeviceSet (..), MsgWithDeviceData (..), forceAttributes, mergeDevice)
import Env (AppM, Env (..), runApp)
import GHC.Base (when)
import Network.Simple.TCP qualified as TCP
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLS
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WS
import Storage (Schedule (..), getCurrentSchedule)

--------------------------------------------------------------------------------

-- | A "ClientApp" is a function that runs once the WebSocket handshake is done.

--------------------------------------------------------------------------------
clientApp :: Env -> WS.Connection -> IO ()
clientApp env conn = do
  putStrLn "WebSocket connected (ignoring cert checks)!"
  -- Spawn a thread to receive messages
  _ <- forkIO $ forever $ do
    runApp env $ do
      msg :: BS.ByteString <- liftIO $ WS.receiveData conn
      case eitherDecode (LBS.fromStrict msg) :: Either String MsgWithDeviceData of
        Left _ -> pure ()
        Right d -> do
          liftIO $ putStrLn $ "[Dirigera] Received Device: " <> show d.deviceData
          let newLight = d.deviceData
          lights <- liftIO $ readTVarIO env.lights
          case M.lookup newLight.id lights of
            Nothing -> do
              liftIO $ atomically $ modifyTVar' env.lights (M.insert newLight.id newLight)
            Just oldLight -> do
              liftIO $ atomically $ modifyTVar' env.lights (M.insert newLight.id (mergeDevice oldLight newLight))
              when
                ( (not oldLight.isReachable && newLight.isReachable)
                    || ((forceAttributes oldLight.attributes).isOn == Just False && (forceAttributes newLight.attributes).isOn == Just True)
                )
                $ do
                  liftIO $ putStrLn "[Dirigera] Light is reachable or turned on, setting brightness and color temperature"
                  let lampId = (head $ fromMaybe [] oldLight.deviceSet).id
                  let lampName = (head $ fromMaybe [] oldLight.deviceSet).name
                  maybeSchedule <- getCurrentSchedule lampId
                  let schedule = fromMaybe (Schedule {scheduleId = 0, lampId = "", timeOfDay = TimeOfDay 0 0 0, brightness = 100, colorTemperature = 3000, allowBrighten = True, allowDarken = True}) maybeSchedule
                  let deviceSet = DeviceSet {name = lampName, id = lampId}
                  setLightLevel deviceSet schedule.brightness 500
                  liftIO $ threadDelay $ 100 * 1000
                  setColorTemperature deviceSet schedule.colorTemperature 500
                  liftIO $ threadDelay $ 100 * 1000

  -- Send a ping every 30s
  forever $ do
    WS.sendTextData conn ("{\"type\":\"ping\"}" :: BS.ByteString)
    threadDelay (30 * 1000000)

--------------------------------------------------------------------------------

-- | connectIgnoringCert:
--   1) Connect via TCP to host:port
--   2) Wrap in TLS, ignoring all certificate checks
--   3) Convert TLS.Context -> WebSockets Stream
--   4) runClientWithStream (passing custom Authorization header)

--------------------------------------------------------------------------------
-- connectIgnoringCert ::
--   -- | Host or IP, e.g. "192.168.178.21"
--   String ->
--   -- | Port, e.g. 8443
--   Int ->
--   -- | Bearer token
--   String ->
--   IO ()
connectIgnoringCert :: AppM ()
connectIgnoringCert = do
  env <- ask
  host <- ipAddr
  let port :: Int = 8443
  bearer <- authToken

  liftIO $ putStrLn $ "Connecting to " ++ T.unpack host ++ " on port " ++ show port ++ ", ignoring cert checks..."

  TCP.connect (T.unpack host) (show port) $ \(sock, _remoteAddr) -> do
    --------------------------------------------------------------------------
    -- 1) Create a TLS context that ignores cert validation
    --------------------------------------------------------------------------
    ctx <- TLS.contextNew sock (clientParams (T.unpack host))
    TLS.handshake ctx

    --------------------------------------------------------------------------
    -- 2) Wrap the TLS context in a 'websockets' Stream
    --------------------------------------------------------------------------
    stream <- liftIO $ makeTLSStream ctx

    --------------------------------------------------------------------------
    -- 3) Provide custom headers (Authorization: Bearer xxxxx)
    --------------------------------------------------------------------------
    let headers =
          [ ( mk (BS8.pack "Authorization"),
              BS8.pack ("Bearer " <> T.unpack bearer)
            )
          ]

    --------------------------------------------------------------------------
    -- 4) Run the WebSocket client with this stream
    --------------------------------------------------------------------------
    liftIO $
      WS.runClientWithStream
        stream -- The 'Stream'
        (T.unpack host)
        "/v1" -- Host, Path
        WS.defaultConnectionOptions
        headers
        (clientApp env)

--------------------------------------------------------------------------------

-- | clientParams: modifies 'TLS.ClientParams' to skip all cert checks

--------------------------------------------------------------------------------
clientParams :: String -> TLS.ClientParams
clientParams serverName =
  (TLS.defaultParamsClient serverName BS.empty)
    { TLS.clientSupported =
        def
          { TLS.supportedCiphers = TLS.ciphersuite_default
          },
      TLS.clientShared =
        def
          { TLS.sharedValidationCache =
              -- Always accept any cert
              TLS.ValidationCache
                (\_ _ _ -> return TLS.ValidationCachePass) -- do not fail
                (\_ _ _ -> return ())
          },
      TLS.clientHooks =
        def
          { -- Also ignore name mismatch or invalid chain
            TLS.onServerCertificate = \_ _ _ _ -> return []
          }
    }

--------------------------------------------------------------------------------

-- | Convert a TLS.Context into a WebSockets 'Stream'

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- makeTLSStream: create a WebSockets Stream from a TLS.Context
-- For websockets versions expecting lazy ByteStrings
--------------------------------------------------------------------------------

makeTLSStream :: TLS.Context -> IO WS.Stream
makeTLSStream ctx = WS.makeStream receive send
  where
    -- 1) Read a strict ByteString
    receive :: IO (Maybe BS.ByteString)
    receive = do
      strictData <- TLS.recvData ctx
      if BS.null strictData
        then return Nothing
        else return (Just strictData)

    -- 2) Write a strict ByteString
    send :: Maybe LBS.ByteString -> IO ()
    send Nothing = return ()
    send (Just bs) = TLS.sendData ctx bs

--                 ^ TLS.sendData needs a lazy ByteString
