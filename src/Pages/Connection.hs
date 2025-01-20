module Pages.Connection (connectionRoutes) where

import Auth qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dirigera (ipAddr, isConnected, isReachable)
import Env (AppM)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (getProperty, updateProperty)
import Web.Scotty.Trans (ScottyT, formParam, get, html, put)

connectionRoutes :: ScottyT AppM ()
connectionRoutes = do
  get "/connection" $ do
    ip <- lift ipAddr
    html $ renderText $ connectionPage ip

  get "/connection/reload" $ do
    ip <- lift ipAddr
    html $ renderText $ connectionContainer ip Loading

  get "/connection/checkReachable" $ do
    ip <- lift ipAddr
    reachable <- lift isReachable
    let rStatus = if reachable then Success else Failed
    html $ renderText $ connectionContainer ip rStatus

  put "/connection/url" $ do
    ip <- formParam "ip"
    lift $ updateProperty "DIRIGERA_IP" ip
    html $ renderText $ connectionContainer ip Loading

  get "/connection/edit" $ do
    url <- lift ipAddr
    html $ renderText $ urlForm url

  get "/connection/status" $ do
    connected <- lift isConnected
    let cStatus = if connected then Success else Failed
    html $ renderText $ connectionStatus cStatus

  get "/connection/promptDirigera" $ do
    html $ renderText tokenLoading

  get "/connection/disconnect" $ do
    lift $ updateProperty "DIRIGERA_TOKEN" ""
    html $ renderText $ connectionStatus Failed

  get "/connection/authorize" $ do
    ip <- lift $ Storage.getProperty "DIRIGERA_IP"
    codeVerifier <- liftIO Auth.generateCodeVerifier
    maybeCode <- liftIO $ Auth.requestAuthCode (fromMaybe "homesmart.local" ip) codeVerifier
    case maybeCode of
      Nothing -> do
        lift $ updateProperty "DIRIGERA_TOKEN" ""
        html $ renderText $ connectionStatus Failed
      Just code -> do
        maybeToken <- liftIO $ Auth.requestAccessToken 59 (fromMaybe "homesmart.local" ip) codeVerifier code
        case maybeToken of
          Nothing -> do
            lift $ updateProperty "DIRIGERA_TOKEN" ""
            html $ renderText $ connectionStatus Failed
          Just tk -> do
            lift $ updateProperty "DIRIGERA_TOKEN" tk
            html $ renderText $ connectionStatus Success

data Status = Loading | Failed | Success deriving (Eq, Show)

connectionPage :: Text -> Html ()
connectionPage ip =
  baseLayout $ connectionContainer ip Loading

connectionContainer :: Text -> Status -> Html ()
connectionContainer ip reached = do
  div_
    [ id_ "connectionContainer",
      class_ "max-w-lg mx-auto p-4 space-y-4"
    ]
    $ do
      div_
        [ class_ "flex flex-col bg-gray-50 p-3 rounded shadow space-y-4",
          hxTarget_ "this",
          hxSwap_ "outerHTML",
          id_ "connectionPage"
        ]
        $ do
          div_ [class_ "flex items-center justify-between"] $ do
            div_ [class_ "flex items-center space-x-2"] $ do
              label_ [class_ "text-gray-700 font-medium"] "URL:"
              span_ [class_ "text-gray-900"] (toHtml ip)
              reachableIcon reached
            button_
              ( [hxGet_ "/connection/edit"]
                  <> if reached == Loading then disabledButton else blueButton
              )
              "Change"
          case reached of
            Success -> connectionStatus Loading
            _ -> pure ()

reachableIcon :: Status -> Html ()
reachableIcon Loading =
  div_
    [ hxGet_ "/connection/checkReachable",
      hxTrigger_ "load",
      hxTarget_ "#connectionContainer",
      hxSwap_ "outerHTML"
    ]
    $ do
      i_ [class_ "fas fa-rotate text-gray-600 animate-spin"] ""
reachableIcon Failed = i_ [class_ "fas fa-link-slash text-red-500"] ""
reachableIcon Success = i_ [class_ "fas fa-link text-green-600"] ""

connectionStatus :: Status -> Html ()
connectionStatus Loading = do
  div_
    [ class_ "flex items-center justify-between",
      hxGet_ "/connection/status",
      hxTrigger_ "load",
      id_ "connectionStatus",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      div_
        [class_ "text-gray-600 animate-pulse"]
        "Trying to connect..."
      button_ disabledButton "Connect"
connectionStatus Failed = do
  div_
    [ class_ "flex items-center justify-between",
      hxTarget_ "this",
      id_ "connectionStatus",
      hxSwap_ "outerHTML"
    ]
    $ do
      div_
        [class_ "text-center text-red-500 text-lg"]
        "Disconnected"
      button_ ([hxGet_ "/connection/promptDirigera"] <> blueButton) "Connect"
connectionStatus Success = do
  div_
    [ class_ "flex items-center justify-between",
      hxTarget_ "this",
      id_ "connectionStatus",
      hxSwap_ "outerHTML"
    ]
    $ do
      div_
        [class_ "text-center text-green-600 text-lg"]
        "Connected"
      button_ ([hxGet_ "/connection/disconnect"] <> redButton) "Disconnect"

tokenLoading :: Html ()
tokenLoading = do
  div_
    [ hxGet_ "/connection/authorize",
      hxTrigger_ "load",
      hxTarget_ "this",
      id_ "connectionStatus",
      hxSwap_ "outerHTML",
      class_ "text-center text-lg text-yellow-600 font-semibold animate-bounce"
    ]
    "Please press Dirigera Action Button!"

urlForm :: Text -> Html ()
urlForm ip = do
  form_
    [ hxPut_ "/connection/url",
      hxTarget_ "#connectionContainer",
      hxSwap_ "outerHTML",
      class_ "bg-white p-4 shadow rounded space-y-4"
    ]
    $ do
      div_ [class_ "flex items-center space-x-2"] $ do
        label_ [class_ "text-gray-700 font-medium"] "URL:"
        input_
          [ type_ "text",
            name_ "ip",
            value_ ip,
            class_ "border border-gray-300 w-full rounded px-2 py-1 focus:outline-none focus:ring focus:ring-blue-100"
          ]
      div_ [class_ "flex justify-end space-x-2"] $ do
        button_
          [ class_ "inline-block rounded-md bg-blue-500 px-3 py-1 text-white hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400",
            type_ "submit"
          ]
          "Submit"
        button_
          [ class_ "inline-block rounded-md bg-gray-500 px-3 py-1 text-white hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-gray-400",
            hxGet_ "/connection/reload"
          ]
          "Cancel"

blueButton :: [Attributes]
blueButton = [class_ "inline-block rounded-md bg-blue-500 px-3 py-1 text-white hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400"]

redButton :: [Attributes]
redButton = [class_ "inline-block rounded-md bg-red-500 px-3 py-1 text-white hover:bg-red-600 focus:outline-none focus:ring-2 focus:ring-red-400"]

disabledButton :: [Attributes]
disabledButton = [class_ "inline-block rounded-md bg-gray-500 px-3 py-1 text-white cursor-not-allowed focus:outline-none focus:ring-2 focus:ring-blue-400", disabled_ ""]
