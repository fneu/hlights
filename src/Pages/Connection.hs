module Pages.Connection (connectionRoutes) where

import Auth qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Text
import Dirigera (authToken, baseURL, isConnected)
import Env (AppM)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (getProperty, updateProperty)
import Web.Scotty.Trans (ScottyT, formParam, get, html, put)

connectionRoutes :: ScottyT AppM ()
connectionRoutes = do
  get "/connection" $ do
    url <- lift baseURL
    token <- lift authToken
    html $ renderText $ connectionPage url token
  get "/connection/url" $ do
    url <- lift baseURL
    token <- lift authToken
    html $ renderText $ connectionContainer url token
  put "/connection/url" $ do
    ip <- formParam "ip"
    lift $ updateProperty "DIRIGERA_IP" ip
    url <- lift baseURL
    token <- lift authToken
    html $ renderText $ connectionContainer url token
  get "/connection/url/edit" $ html $ renderText urlForm
  get "/connection/status" $ do
    connected <- lift isConnected
    html $ renderText $ connectedDisplay connected
  get "/connection/disconnect" $ do
    lift $ updateProperty "DIRIGERA_TOKEN" ""
    url <- lift baseURL
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
          Just token -> do
            lift $ updateProperty "DIRIGERA_TOKEN" token
            url <- lift baseURL
            html $ renderText $ connectionContainer url token

connectionPage :: Text -> Text -> Html ()
connectionPage url token = baseLayout $ connectionContainer url token

connectionContainer :: Text -> Text -> Html ()
connectionContainer url token = do
  div_ [id_ "connectionContainer"] $ do
    urlDisplay url
    tokenDisplay token
    div_ [hxGet_ "/connection/status", hxTrigger_ "load", id_ "statusArea"] "loading..."

urlDisplay :: Text -> Html ()
urlDisplay url = do
  div_ [hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "URL: "
      toHtml url
    button_ [class_ "btn btn-primary", hxGet_ "/connection/url/edit"] "Change"

urlForm :: Html ()
urlForm = do
  form_ [hxPut_ "/connection/url", hxTarget_ "#connectionContainer", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "URL: https://"
      input_ [type_ "text", name_ "ip", value_ ""]
      label_ ":8443/v1"
    button_ [class_ "btn btn-primary", type_ "submit"] "Submit"
    button_ [class_ "btn btn-secondary", hxGet_ "/connection/url"] "Cancel"

tokenDisplay :: Text -> Html ()
tokenDisplay token = do
  div_ [hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "Token acquired: "
      if token == "" then "No" else "Yes"
    button_ [class_ "btn btn-primary", hxGet_ "/connection/connect"] $ if token == "" then "Connect" else "Reconnect"
    button_ [class_ "btn btn-primary", hxGet_ "/connection/disconnect", hxTarget_ "#connectionContainer"] "Disconnect"

tokenLoading :: Html ()
tokenLoading = do
  div_ [hxGet_ "/connection/connectRequest", hxTrigger_ "load", hxTarget_ "#connectionContainer"] "Please press Dirigera Action Button!"

connectedDisplay :: Bool -> Html ()
connectedDisplay isCon = do
  h1_ ([class_ "text-3xl font-bold mb-4"] <> conClass) conText
  button_ [class_ "btn btn-primary", hxGet_ "/connection/url", hxTarget_ "#connectionContainer"] "Refresh"
  where
    conClass = if isCon then [class_ "text-green-500"] else [class_ "text-red-500"]
    conText = if isCon then "Connected" else "Disconnected"
