module Pages.Home (homeRoutes, homePage) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dirigera (switchIsOn)
import Dirigera.Devices
import Env (AppM, Env (..))
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Web.Scotty.Trans (ScottyT, get, html, queryParam)

homeRoutes :: ScottyT AppM ()
homeRoutes = do
  get "/home" $ do
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    let lamps = nub $ concatMap (.deviceSet) (M.elems lights)
    html $ renderText $ homePage lamps lights
  get "/home/switchLamp" $ do
    lampId <- queryParam "id"
    lampName <- queryParam "name"
    toOn <- queryParam "toOn"
    let deviceSet = DeviceSet {name = lampName, id = lampId}
    lift $ switchIsOn deviceSet toOn
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    html $ renderText $ lampCard deviceSet lights

homePage :: [DeviceSet] -> M.Map Text Device -> Html ()
homePage lamps lights = baseLayout $ do
  div_ [class_ "max-w-lg mx-auto p-4 space-y-4"] $ do
    forM_ lamps $ \lamp -> lampCard lamp lights

lampCard :: DeviceSet -> M.Map Text Device -> Html ()
lampCard lamp lights = do
  let lampDevices = filter (\d -> lamp `elem` d.deviceSet) (M.elems lights)
  -- TODO: Implement schedule check
  let onSchedule = lamp.name == "Wohnzimmerlampe"
  let isOn = any (\d -> fromMaybe False d.attributes.isOn) lampDevices
  div_
    [ class_ "flex flex-col bg-gray-50 p-3 rounded shadow space-y-4",
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
        button_ [class_ "inline-block rounded-md px-3 py-1 flex-grow h-12 bg-white shadow-md border border-gray-300"] "Schedule"
        if not onSchedule
          then button_ [class_ "inline-block rounded-md px-3 py-1 w-12 h-12 bg-white shadow-md border border-gray-300"] $ do
            i_ [class_ "fas fa-clock-rotate-left text-gray-600"] ""
          else mempty
      div_ [class_ "flex items-center justify-between space-x-4"] $ do
        if isOn
          then ovalButton "power-off" ("/home/switchLamp?toOn=false&id=" <> lamp.id <> "&name=" <> lamp.name) "on"
          else ovalButton "power-off" ("/home/switchLamp?toOn=true&id=" <> lamp.id <> "&name=" <> lamp.name) "off"
        ovalButton "palette" "/connection" "1400"
        ovalButtonWithDropdown "sun" "/connection" "89%"

ovalButton :: Text -> Text -> Text -> Html ()
ovalButton icon url info = do
  button_
    [ class_ "flex items-center justify-between w-20 sm:w-36 h-12 px-3 rounded-full bg-white shadow-md border border-gray-300",
      hxGet_ url
    ]
    $ do
      i_ [class_ $ "fas fa-" <> icon <> " text-gray-600"] ""
      span_ [class_ "flex-grow text-center text-gray-800"] $ toHtml info

-- A button with embedded dropdown for brightness control
ovalButtonWithDropdown :: Text -> Text -> Text -> Html ()
ovalButtonWithDropdown icon url info = do
  div_ [class_ "relative inline-block w-20 sm:w-36"] $ do
    button_
      [ class_ "flex items-center justify-between w-full h-12 px-3 rounded-full bg-white shadow-md border border-gray-300"
      ]
      $ do
        i_ [class_ $ "fas fa-" <> icon <> " text-gray-600"] ""
        span_ [class_ "flex-grow text-center text-gray-800"] $ toHtml info

    select_
      [ name_ "brightness",
        class_ "absolute top-0 left-0 w-full h-full opacity-0 cursor-pointer",
        hxPost_ url,
        hxTrigger_ "change",
        hxParams_ "value"
      ]
      $ do
        option_ [value_ "0"] "0%"
        option_ [value_ "10"] "10%"
        option_ [value_ "20"] "20%"
        option_ [value_ "30"] "30%"
        option_ [value_ "40"] "40%"
        option_ [value_ "50"] "50%"
        option_ [value_ "60"] "60%"
        option_ [value_ "70"] "70%"
        option_ [value_ "80"] "80%"
        option_ [value_ "90"] "90%"
        option_ [value_ "100"] "100%"
