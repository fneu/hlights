module Scheduler where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, asks)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Time
  ( TimeOfDay (..),
    getCurrentTime,
  )
import Dirigera (setColorTemperature, setLightLevel)
import Dirigera.Devices (Attributes (..), Device (..), DeviceSet (..))
import Env (AppM, Env (..), runApp)
import Storage (Schedule (..), listSchedulesByMinute, timeOfDayToMinutes, utcToLocalTime)

startScheduleManager :: AppM ()
startScheduleManager = do
  env <- ask
  lastMinuteRef <- liftIO $ newIORef (-1)

  _ <- liftIO $ forkIO $ forever $ do
    nowLocal@(TimeOfDay _ _ sec) <- getCurrentTime >>= utcToLocalTime
    let currentMinute = timeOfDayToMinutes nowLocal
    lastMinute <- readIORef lastMinuteRef
    when (currentMinute /= lastMinute) $ do
      writeIORef lastMinuteRef currentMinute
      runApp env $ do
        schedules <- listSchedulesByMinute currentMinute
        mapM_ applySchedule schedules

    let fractionalSeconds = realToFrac sec :: Double
        secToNextMin = 60 - fractionalSeconds
        microToWait = floor (secToNextMin * 1000000) + 50_000

    threadDelay microToWait
  pure ()

applySchedule :: Schedule -> AppM ()
applySchedule schedule = do
  let lampId = schedule.lampId
  envLights <- asks (.lights)
  lights <- liftIO $ readTVarIO envLights
  let lamps = nub $ concatMap (.deviceSet) (M.elems lights)
  let maybeLamps = filter (\lamp -> lamp.id == lampId) lamps
  unless (null maybeLamps) $ do
    let deviceSet = head maybeLamps
    let lampDevices = filter (\d -> deviceSet `elem` d.deviceSet) (M.elems lights)
    let currentBrightness = fromMaybe 0 (head lampDevices).attributes.lightLevel

    let targetBrightness = schedule.brightness
    let targetColorTemperature = schedule.colorTemperature
    let allowBrighten = schedule.allowBrighten
    let allowDarken = schedule.allowDarken
    when
      ( (currentBrightness < targetBrightness && allowBrighten)
          || (currentBrightness > targetBrightness && allowDarken)
      )
      $ setLightLevel deviceSet targetBrightness 500
    liftIO $ threadDelay $ 200 * 1000
    setColorTemperature deviceSet targetColorTemperature 500
    liftIO $ threadDelay $ 200 * 1000
