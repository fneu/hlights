module Pages.Connection (connectionRoutes) where

import Auth qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dirigera (authToken, ipAddr, isConnected)
import Env (AppM)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (getProperty, updateProperty)
import Web.Scotty.Trans (ScottyT, formParam, get, html, put)

connectionRoutes :: ScottyT AppM ()
connectionRoutes = do
  get "/connection" $ do
    url <- lift ipAddr
    token <- lift authToken
    html $ renderText $ connectionPage url token

  get "/connection/url" $ do
    url <- lift ipAddr
    token <- lift authToken
    html $ renderText $ connectionContainer url token

  put "/connection/url" $ do
    ip <- formParam "ip"
    lift $ updateProperty "DIRIGERA_IP" ip
    url <- lift ipAddr
    token <- lift authToken
    html $ renderText $ connectionContainer url token

  get "/connection/url/edit" $ do
    url <- lift ipAddr
    html $ renderText $ urlForm url

  get "/connection/status" $ do
    connected <- lift isConnected
    html $ renderText $ connectedDisplay connected

  get "/connection/disconnect" $ do
    lift $ updateProperty "DIRIGERA_TOKEN" ""
    url <- lift ipAddr
    html $ renderText $ connectionContainer url ""

  get "/connection/connect" $ do
    html $ renderText tokenLoading

  get "/connection/connectRequest" $ do
    ip <- lift $ Storage.getProperty "DIRIGERA_IP"
    codeVerifier <- liftIO Auth.generateCodeVerifier
    maybeCode <- liftIO $ Auth.requestAuthCode (fromMaybe "homesmart.local" ip) codeVerifier
    case maybeCode of
      Nothing -> do
        lift $ updateProperty "DIRIGERA_TOKEN" ""
        html $ renderText $ tokenDisplay ""
      Just code -> do
        maybeToken <- liftIO $ Auth.requestAccessToken 59 (fromMaybe "homesmart.local" ip) codeVerifier code
        case maybeToken of
          Nothing -> do
            lift $ updateProperty "DIRIGERA_TOKEN" ""
            html $ renderText $ tokenDisplay ""
          Just tk -> do
            lift $ updateProperty "DIRIGERA_TOKEN" tk
            url <- lift ipAddr
            html $ renderText $ connectionContainer url tk

connectionPage :: Text -> Text -> Html ()
connectionPage url token =
  baseLayout $ connectionContainer url token

connectionContainer :: Text -> Text -> Html ()
connectionContainer url token = do
  div_
    [ id_ "connectionContainer",
      class_ "max-w-lg mx-auto p-4 space-y-4"
    ]
    $ do
      urlDisplay url
      tokenDisplay token
      div_
        [ hxGet_ "/connection/status",
          hxTrigger_ "load",
          hxTarget_ "this",
          hxSwap_ "outerHTML",
          id_ "statusArea",
          class_ "text-center text-gray-600 animate-pulse"
        ]
        "Loading..."

urlDisplay :: Text -> Html ()
urlDisplay url = do
  div_
    [ class_ "flex items-center justify-between bg-gray-50 p-3 rounded shadow",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      div_ [class_ "flex items-center space-x-2"] $ do
        label_ [class_ "text-gray-700 font-medium"] "URL:"
        span_ [class_ "text-gray-900"] (toHtml url)
      button_
        [ class_ "inline-block rounded-md bg-blue-500 px-3 py-1 text-white hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400",
          hxGet_ "/connection/url/edit"
        ]
        "Change"

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
            hxGet_ "/connection/url"
          ]
          "Cancel"

tokenDisplay :: Text -> Html ()
tokenDisplay token = do
  div_
    [ class_ "flex items-center justify-between bg-gray-50 p-3 rounded shadow",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      let hasToken = token /= ""
      div_ [class_ "flex items-center space-x-2"] $ do
        label_ [class_ "text-gray-700 font-medium"] "Token acquired:"
        span_
          [class_ (if hasToken then "text-green-600" else "text-red-500")]
          (if hasToken then "Yes" else "No")
      if hasToken
        then
          button_
            [ class_ "inline-block rounded-md bg-red-500 px-3 py-1 text-white hover:bg-red-600 focus:outline-none focus:ring-2 focus:ring-red-400",
              hxGet_ "/connection/disconnect",
              hxTarget_ "#connectionContainer",
              hxSwap_ "outerHTML"
            ]
            "Disconnect"
        else
          button_
            [ class_ "inline-block rounded-md bg-blue-500 px-3 py-1 text-white hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-400",
              hxGet_ "/connection/connect"
            ]
            "Connect"

tokenLoading :: Html ()
tokenLoading = do
  div_
    [ hxGet_ "/connection/connectRequest",
      hxTrigger_ "load",
      hxTarget_ "#connectionContainer",
      hxSwap_ "outerHTML",
      class_ "text-center text-lg text-yellow-600 font-semibold animate-bounce p-4"
    ]
    "Please press Dirigera Action Button!"

connectedDisplay :: Bool -> Html ()
connectedDisplay isCon = do
  let (statusColor, statusText) =
        if isCon
          then ("text-green-500", "Connected")
          else ("text-red-500", "Disconnected")

  div_ [class_ "text-center"] $ do
    h1_ [class_ $ "text-2xl font-bold mb-4 " <> statusColor] statusText

    button_
      [ class_ "inline-block rounded-md bg-gray-500 px-3 py-1 text-white hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-gray-400",
        hxGet_ "/connection/url",
        hxTarget_ "#connectionContainer",
        hxSwap_ "outerHTML"
      ]
      "Refresh"
