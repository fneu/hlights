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
    html $ renderText $ urlDisplay url
  put "/connection/url" $ do
    ip <- formParam "ip"
    lift $ updateProperty "DIRIGERA_IP" ip
    url <- lift baseURL
    html $ renderText $ urlDisplay url
  get "/connection/url/edit" $ html $ renderText urlForm
  get "/connection/status" $ do
    connected <- lift isConnected
    html $ renderText $ connectedDisplay connected
  get "/connection/disconnect" $ do
    lift $ updateProperty "DIRIGERA_TOKEN" ""
    token <- lift authToken
    html $ renderText $ tokenDisplay token
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
            html $ renderText $ tokenDisplay token

connectionPage :: Text -> Text -> Html ()
connectionPage url token = baseLayout $ do
  urlDisplay url
  tokenDisplay token
  div_ [hxGet_ "/connection/status", hxTrigger_ "load"] "loading..."

urlDisplay :: Text -> Html ()
urlDisplay url = do
  div_ [hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "URL: "
      toHtml url
    button_ [class_ "btn btn-primary", hxGet_ "/connection/url/edit"] "Change"

tokenDisplay :: Text -> Html ()
tokenDisplay token = do
  div_ [hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "Token acquired: "
      if token == "" then "No" else "Yes"
    button_ [class_ "btn btn-primary", hxGet_ "/connection/connect"] $ if token == "" then "Connect" else "Reconnect"
    button_ [class_ "btn btn-primary", hxGet_ "/connection/disconnect"] "Disconnect"

tokenLoading :: Html ()
tokenLoading = do
  div_ [hxGet_ "/connection/connectRequest", hxTrigger_ "load"] "Please press Dirigera Action Button!"

urlForm :: Html ()
urlForm = do
  form_ [hxPut_ "/connection/url", hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "URL: https://"
      input_ [type_ "text", name_ "ip", value_ ""]
      label_ ":8443/v1"
    button_ [class_ "btn btn-primary", type_ "submit"] "Submit"
    button_ [class_ "btn btn-secondary", hxGet_ "/connection/url"] "Cancel"

connectedDisplay :: Bool -> Html ()
connectedDisplay True = h1_ [class_ "text-3xl font-bold mb-4 text-green-500"] "Connected"
connectedDisplay False = h1_ [class_ "text-3xl font-bold mb-4 text-red-500"] "Disconnected"
