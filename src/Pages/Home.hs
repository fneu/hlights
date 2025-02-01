module Pages.Home (homeRoutes, homePage) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, asks)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as TL
import Data.Time
import Dirigera (fetchLights, setColorTemperature, setLightLevel, switchIsOn)
import Dirigera.Devices
import Env (AppM, Env (..), runApp)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (Schedule (..), getCurrentSchedule)
import Web.Scotty.Trans (ScottyT, get, html, queryParam, redirect)

homeRoutes :: ScottyT AppM ()
homeRoutes = do
  get "/home" $ do
    env <- lift ask
    envLights <- lift $ asks (.lights)
    fetchedLights <- lift fetchLights
    liftIO $ atomically $ writeTVar envLights fetchedLights
    let lamps = nub $ concatMap (\l -> fromMaybe [] l.deviceSet) (M.elems fetchedLights)
    let lampSchedulesActions = map (\lamp -> (lamp.id, getCurrentSchedule lamp.id)) lamps
    lampSchedules <- liftIO $ do
      results <-
        mapM
          ( \(lampId, action) -> do
              sched <- runApp env action
              return (lampId, sched)
          )
          lampSchedulesActions
      return (M.fromList results)
    html $ renderText $ baseLayout $ homePage lamps fetchedLights lampSchedules
  get "/home/switchLamp" $ do
    lampId <- queryParam "id"
    lampName <- queryParam "name"
    toOn <- queryParam "toOn"
    let deviceSet = DeviceSet {name = lampName, id = lampId}
    lift $ switchIsOn deviceSet toOn
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    schedule <- lift $ getCurrentSchedule lampId
    if toOn
      then redirect $ "/home/resetToSchedule?id=" <> TL.fromStrict lampId <> "&name=" <> TL.fromStrict lampName
      else html $ renderText $ lampCard deviceSet lights schedule
  get "/home/setLightLevel" $ do
    lampId <- queryParam "id"
    lampName <- queryParam "name"
    lightLevel <- queryParam "value"
    let deviceSet = DeviceSet {name = lampName, id = lampId}
    lift $ setLightLevel deviceSet lightLevel 500
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    schedule <- lift $ getCurrentSchedule lampId
    html $ renderText $ lampCard deviceSet lights schedule
  get "/home/setColorTemperature" $ do
    lampId <- queryParam "id"
    lampName <- queryParam "name"
    colorTemperature <- queryParam "value"
    let deviceSet = DeviceSet {name = lampName, id = lampId}
    lift $ setColorTemperature deviceSet colorTemperature 500
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    schedule <- lift $ getCurrentSchedule lampId
    html $ renderText $ lampCard deviceSet lights schedule
  get "/home/resetToSchedule" $ do
    lampId <- queryParam "id"
    lampName <- queryParam "name"
    maybeSchedule <- lift $ getCurrentSchedule lampId
    let schedule = fromMaybe (Schedule {scheduleId = 0, lampId = "", timeOfDay = TimeOfDay 0 0 0, brightness = 100, colorTemperature = 3000, allowBrighten = True, allowDarken = True}) maybeSchedule
    let deviceSet = DeviceSet {name = lampName, id = lampId}
    lift $ setLightLevel deviceSet schedule.brightness 500
    lift $ setColorTemperature deviceSet schedule.colorTemperature 500
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    html $ renderText $ lampCard deviceSet lights maybeSchedule

homePage :: [DeviceSet] -> M.Map Text Device -> M.Map Text (Maybe Schedule) -> Html ()
homePage lamps lights schedules = do
  div_
    [ id_ "homePage",
      class_ "flex flex-col items-center md:items-center md:max-w-2xl mx-auto p-4 space-y-4 w-full"
    ]
    $ do
      forM_ lamps $ \lamp -> lampCard lamp lights (fromMaybe Nothing (M.lookup lamp.id schedules))

lampCard :: DeviceSet -> M.Map Text Device -> Maybe Schedule -> Html ()
lampCard lamp lights maybeSchedule = do
  let lampDevices = filter (\d -> lamp `elem` fromMaybe [] d.deviceSet) (M.elems lights)
  let isOn = any (\d -> fromMaybe False (forceAttributes d.attributes).isOn) lampDevices
  let isReachable = any (\d -> d.isReachable) lampDevices
  let lightLevel = fromMaybe 0 (forceAttributes (head lampDevices).attributes).lightLevel
  let colorTemperature = fromMaybe 0 (forceAttributes (head lampDevices).attributes).colorTemperature
  let onSchedule = case maybeSchedule of
        Just schedule -> colorTemperature == schedule.colorTemperature && lightLevel == schedule.brightness
        Nothing -> False
  div_
    [ class_ "flex flex-col bg-gray-50 p-3 rounded shadow space-y-4 w-full",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "text-lg"] $ toHtml lamp.name
        div_ [class_ "flex items-center text-gray-500 space-x-1 border border-gray-300 rounded-full px-2"] $ do
          span_ [class_ "text-lg"] $ toHtml $ show $ length lampDevices
          i_ [class_ "far fa-lightbulb"] ""
      div_ [class_ "flex items-center justify-center space-x-4"] $ do
        a_
          [ class_ "inline-block rounded-md px-3 py-1 flex-grow h-12 bg-white shadow-md border border-gray-300 text-center leading-[2.5rem]",
            href_ ("/home/schedule?lampId=" <> lamp.id)
          ]
          "Schedule"
        if not onSchedule
          then button_
            [ class_ "inline-block rounded-md px-3 py-1 w-12 h-12 bg-white shadow-md border border-gray-300",
              hxGet_ ("/home/resetToSchedule?id=" <> lamp.id <> "&name=" <> lamp.name)
            ]
            $ do
              i_ [class_ "fas fa-clock-rotate-left text-gray-600"] ""
          else mempty
      div_ [class_ "flex items-center justify-between"] $ do
        if isOn
          then onOffButton ("/home/switchLamp?toOn=false&id=" <> lamp.id <> "&name=" <> lamp.name) "on" (not isReachable)
          else onOffButton ("/home/switchLamp?toOn=true&id=" <> lamp.id <> "&name=" <> lamp.name) "off" (not isReachable)
        ovalButtonWithDropdown
          "palette"
          ("/home/setColorTemperature?id=" <> lamp.id <> "&name=" <> lamp.name)
          ((pack . show $ colorTemperature) <> "K")
          [ (4000, "4000K"),
            (3700, "3700K"),
            (3400, "3400K"),
            (3100, "3100K"),
            (2800, "2800K"),
            (2500, "2500K"),
            (2202, "2202K")
          ]
          (not isOn || not isReachable)
        ovalButtonWithDropdown
          "sun"
          ("/home/setLightLevel?id=" <> lamp.id <> "&name=" <> lamp.name)
          ((pack . show $ lightLevel) <> "%")
          [ (100, "100%"),
            (90, "90%"),
            (80, "80%"),
            (70, "70%"),
            (60, "60%"),
            (50, "50%"),
            (40, "40%"),
            (30, "30%"),
            (20, "20%"),
            (10, "10%"),
            (1, "1%")
          ]
          (not isOn || not isReachable)

onOffButton :: Text -> Text -> Bool -> Html ()
onOffButton _ _ True = do
  button_
    [ class_ "flex items-center justify-between w-24 sm:w-36 h-12 px-3 rounded-full bg-gray-200 text-gray-500 cursor-not-allowed",
      disabled_ ""
    ]
    $ do
      i_ [class_ "fas fa-power-off"] ""
      span_ [class_ "flex-grow text-center"] "offline"
onOffButton url info False = do
  button_
    [ class_ "flex items-center justify-between w-24 sm:w-36 h-12 px-3 rounded-full bg-white shadow-md border border-gray-300",
      hxGet_ url
    ]
    $ do
      i_ [class_ "fas fa-power-off text-gray-600"] ""
      span_ [class_ "flex-grow text-center text-gray-800"] $ toHtml info

ovalButtonWithDropdown :: Text -> Text -> Text -> [(Int, Text)] -> Bool -> Html ()
ovalButtonWithDropdown icon _ info _ True = do
  button_
    [ class_ "flex items-center justify-between w-24 sm:w-36 h-12 px-3 rounded-full bg-gray-200 text-gray-500 cursor-not-allowed",
      disabled_ ""
    ]
    $ do
      i_ [class_ $ "fas fa-" <> icon] ""
      span_ [class_ "flex-grow text-center"] $ toHtml info
ovalButtonWithDropdown icon url info options False = do
  div_ [class_ "relative inline-block w-24 sm:w-36"] $ do
    button_
      [ class_ "flex items-center justify-between w-full h-12 px-3 rounded-full bg-white shadow-md border border-gray-300"
      ]
      $ do
        i_ [class_ $ "fas fa-" <> icon <> " text-gray-600"] ""
        span_ [class_ "flex-grow text-center text-gray-800"] $ toHtml info

    select_
      [ name_ "value",
        class_ "absolute top-0 left-0 w-full h-full opacity-0 cursor-pointer",
        hxGet_ url
      ]
      $ do
        option_ [value_ "1", selected_ "", disabled_ "", hidden_ ""] "-- Select --"
        forM_ options $ \(value, text) -> do
          option_ [value_ (pack . show $ value)] $ toHtml text
