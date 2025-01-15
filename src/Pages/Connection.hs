module Pages.Connection (connectionRoutes) where

import Control.Monad.Trans.Class (lift)
import Data.Text
import Dirigera (authToken, baseURL, isConnected)
import Env (AppM)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (updateProperty)
import Web.Scotty.Trans (ScottyT, formParam, get, html, put)

connectionRoutes :: ScottyT AppM ()
connectionRoutes = do
  get "/connection" $ do
    url <- lift baseURL
    token <- lift authToken
    connected <- lift isConnected
    html $ renderText $ connectionPage url token connected
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

connectionPage :: Text -> Text -> Bool -> Html ()
connectionPage url token connected = baseLayout $ do
  urlDisplay url
  h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml token)
  div_ [hxGet_ "/connection/status", hxTrigger_ "load"] "loading..."

urlDisplay :: Text -> Html ()
urlDisplay url = do
  div_ [hxTarget_ "this", hxSwap_ "outerHTML"] $ do
    div_ $ do
      label_ "URL: "
      toHtml url
    button_ [class_ "btn btn-primary", hxGet_ "/connection/url/edit"] "Change"

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
