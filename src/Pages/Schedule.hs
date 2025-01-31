module Pages.Schedule
  ( scheduleRoutes,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Time.LocalTime
import Dirigera (setColorTemperature, setLightLevel)
import Dirigera.Devices (Device (..), DeviceSet (..))
import Env (AppM, Env (..))
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Network.HTTP.Types.Status (status204)
import Storage
  ( Schedule (..),
    addSchedule,
    deleteSchedule,
    listSchedulesByLampId,
  )
import Text.Read (readMaybe)
import Web.Scotty.Trans (ScottyT, formParam, formParams, get, html, post, queryParam, redirect, status)

--------------------------------------------------------------------------------
-- 1) The Routes
--------------------------------------------------------------------------------

scheduleRoutes :: ScottyT AppM ()
scheduleRoutes = do
  ------------------------------------------------------------------------------
  -- GET /home/schedule?lampId=xxxx
  ------------------------------------------------------------------------------
  get "/home/schedule" $ do
    lampId <- queryParam "lampId"
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    let validLamps = nub $ concatMap (.deviceSet) (M.elems lights)
    if lampId `elem` map (.id) validLamps
      then do
        schedules <- lift $ listSchedulesByLampId lampId
        html $ renderText $ baseLayout $ schedulePage lampId schedules
      else redirect "/home"

  ------------------------------------------------------------------------------
  -- POST /home/schedule/add
  ------------------------------------------------------------------------------
  post "/home/schedule/add" $ do
    -- We expect form fields named "lampId", "timeHHMM", "brightness",
    -- "colorTemperature", "allowBrighten", and "allowDarken".
    --
    -- For checkboxes: if they are unchecked, they won't appear in 'params'.
    -- So we use maybeBool to interpret them as True/False.

    lampId <- formParam "lampId"
    timeHHMM <- formParam "timeHHMM"
    brightnessStr <- formParam "brightness"
    ctStr <- formParam "colorTemperature"

    allParams <- formParams
    -- Checkboxes might not be present if unchecked, so look them up manually:
    let allowBrighten = maybeBool $ lookup "allowBrighten" allParams
        allowDarken = maybeBool $ lookup "allowDarken" allParams

    -- Parse the HH:MM -> TimeOfDay -> minutes
    let maybeTOD = parseHHMM timeHHMM
        brightness = fromMaybe 100 (readMaybe $ T.unpack brightnessStr)
        colorTemp = fromMaybe 3700 (readMaybe $ T.unpack ctStr)

    case maybeTOD of
      Nothing -> do
        -- If we fail to parse time, we can redirect or show an error.
        -- For simplicity, let's just redirect back.
        redirect ("/home/schedule?lampId=" <> lampId)
      Just tod -> do
        -- Insert into DB
        _ <- lift $ addSchedule (toStrict lampId) tod brightness colorTemp allowBrighten allowDarken
        redirect ("/home/schedule?lampId=" <> lampId)

  ------------------------------------------------------------------------------
  -- POST /home/schedule/delete
  ------------------------------------------------------------------------------
  post "/home/schedule/delete" $ do
    scheduleId <- formParam "scheduleId"
    lampId <- formParam "lampId" -- so we can redirect back to same lamp
    _ <- lift $ deleteSchedule scheduleId
    redirect ("/home/schedule?lampId=" <> lampId)

  post "/home/schedule/try" $ do
    lampId <- queryParam "lampId"
    brightness <- formParam "brightness"
    colorTemperature <- formParam "colorTemperature"

    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    let lamps = nub $ concatMap (.deviceSet) (M.elems lights)
    let lamp = head $ filter (\l -> l.id == lampId) lamps

    lift $ setLightLevel lamp brightness 500
    liftIO $ threadDelay $ 200 * 1000
    lift $ setColorTemperature lamp colorTemperature 500

    status status204

-- (Optional) If you want an update route, you can add something similar:
-- post "/home/schedule/update" $ do
--   ... parse fields ...
--   updateSchedule ...
--   redirect ("/home/schedule?lampId=" <> lampId)

--------------------------------------------------------------------------------
-- 2) The Page HTML
--------------------------------------------------------------------------------

-- | A page that lists all schedules for a given lamp,
--   plus a form to add a new schedule.
schedulePage :: Text -> [Schedule] -> Html ()
schedulePage lampId schedules = do
  div_ [class_ "p-4 space-y-4"] $ do
    h1_ [class_ "text-2xl font-bold"] $ "Schedules for Lamp: " <> toHtml lampId

    -- A list/table of existing schedules
    table_ [class_ "min-w-full border border-gray-300"] $ do
      thead_ $ tr_ $ do
        th_ [class_ "border px-2 py-1"] "Time"
        th_ [class_ "border px-2 py-1"] "Brightness"
        th_ [class_ "border px-2 py-1"] "Color Temp"
        th_ [class_ "border px-2 py-1"] "Brighten?"
        th_ [class_ "border px-2 py-1"] "Darken?"
        th_ [class_ "border px-2 py-1"] "Action"
      tbody_ $ mapM_ (scheduleRow lampId) schedules

    -- A form to add a new schedule
    newScheduleForm lampId

-- | Render one row for an existing schedule.
scheduleRow :: Text -> Schedule -> Html ()
scheduleRow lampId sch = do
  let hhmm = formatHHMM sch.timeOfDay
  tr_ [class_ "border"] $ do
    td_ [class_ "border px-2 py-1"] (toHtml hhmm)
    td_ [class_ "border px-2 py-1"] (toHtml $ show sch.brightness <> "%")
    td_ [class_ "border px-2 py-1"] (toHtml $ show sch.colorTemperature <> "K")
    td_ [class_ "border px-2 py-1"] (toHtml $ if sch.allowBrighten then "Yes" else "No" :: Text)
    td_ [class_ "border px-2 py-1"] (toHtml $ if sch.allowDarken then "Yes" else "No" :: Text)
    td_ [class_ "border px-2 py-1"] $ do
      -- A simple "Delete" button that POSTs to /home/schedule/delete
      form_ [method_ "post", action_ "/home/schedule/delete"] $ do
        input_ [type_ "hidden", name_ "scheduleId", value_ (pack $ show sch.scheduleId)]
        input_ [type_ "hidden", name_ "lampId", value_ lampId]
        button_ [type_ "submit", class_ "text-red-600 underline"] "Delete"

-- (Similarly, you could add an "Edit" button that leads to a separate form.)

-- | Form to add a new schedule for this lamp
newScheduleForm :: Text -> Html ()
newScheduleForm lampId = do
  h2_ [class_ "text-xl font-semibold mt-4"] "Add a new schedule"
  form_ [method_ "post", action_ "/home/schedule/add", class_ "space-y-2", id_ "scheduleForm"] $ do
    -- hidden field so we know which lamp
    input_ [type_ "hidden", name_ "lampId", value_ lampId]

    div_ [class_ "flex items-center space-x-2"] $ do
      label_ [class_ "w-24"] "Time (HH:MM)"
      input_ [type_ "text", name_ "timeHHMM", class_ "border px-2 py-1", placeholder_ "07:30"]

    div_ [class_ "flex items-center space-x-2"] $ do
      label_ [class_ "w-24"] "Brightness (%)"
      input_ [type_ "number", name_ "brightness", class_ "border px-2 py-1", value_ "100", min_ "1", max_ "100"]

    div_ [class_ "flex items-center space-x-2"] $ do
      label_ [class_ "w-24"] "Color Temp (K)"
      input_ [type_ "number", name_ "colorTemperature", class_ "border px-2 py-1", value_ "3700", min_ "1000", max_ "10000"]

    div_ [class_ "flex items-center space-x-4"] $ do
      div_ [class_ "flex items-center space-x-1"] $ do
        input_ [type_ "checkbox", name_ "allowBrighten", value_ "true"]
        label_ "Allow Brighten"
      div_ [class_ "flex items-center space-x-1"] $ do
        input_ [type_ "checkbox", name_ "allowDarken", value_ "true"]
        label_ "Allow Darken"
    button_ [type_ "button", hxPost_ ("/home/schedule/try?lampId=" <> lampId), hxInclude_ "#scheduleForm", hxSwap_ "none", class_ "bg-green-600 text-white px-4 py-1 rounded"] "Try Settings"

    button_ [type_ "submit", class_ "bg-blue-600 text-white px-4 py-1 rounded"] "Add Schedule"

--------------------------------------------------------------------------------
-- 3) Helpers: parse / format HH:MM
--------------------------------------------------------------------------------

-- | Convert a 'TimeOfDay' to a "HH:MM" string, zero-padded.
formatHHMM :: TimeOfDay -> String
formatHHMM (TimeOfDay hh mm _) =
  let hh' = if hh < 10 then "0" <> show hh else show hh
      mm' = if mm < 10 then "0" <> show mm else show mm
   in hh' <> ":" <> mm'

-- | Parse a "HH:MM" string into a 'TimeOfDay', ignoring seconds.
parseHHMM :: Text -> Maybe TimeOfDay
parseHHMM t =
  let s = T.unpack t
   in case break (== ':') s of
        (hh, ':' : mm) ->
          case (readMaybe hh, readMaybe mm) of
            (Just h, Just m) ->
              makeTimeOfDayValid h (fromIntegral (m :: Int)) 0
            _ -> Nothing
        _ -> Nothing

-- | Convert a maybe checkbox parameter to a Bool.
maybeBool :: Maybe Text -> Bool
maybeBool (Just "true") = True
maybeBool (Just "false") = False
maybeBool _ = False
