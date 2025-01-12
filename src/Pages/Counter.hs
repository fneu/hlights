module Pages.Counter (counterRoutes, counterPage) where

import Data.Text
import Database.SQLite.Simple
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (getProperty, updateProperty)
import Web.Scotty

getCounter :: Connection -> IO Int
getCounter conn = do
  prop <- getProperty conn "counter"
  pure $ maybe 0 read (prop >>= Just . unpack)

updateCounter :: Connection -> Int -> IO ()
updateCounter conn = updateProperty conn "counter" . pack . show

counterRoutes :: Connection -> ScottyM ()
counterRoutes conn = do
  get "/counter" $ do
    counter <- liftIO $ getCounter conn
    html $ renderText $ counterPage counter

  get "/counter/increment" $ do
    counter <- liftIO $ getCounter conn
    let newCounter = counter + 1
    liftIO $ updateCounter conn newCounter
    html . renderText $
      h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ newCounter)

  get "/counter/decrement" $ do
    counter <- liftIO $ getCounter conn
    let newCounter = counter - 1
    liftIO $ updateCounter conn newCounter
    html . renderText $
      h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ newCounter)

counterPage :: Int -> Html ()
counterPage counter = baseLayout $ do
  h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ counter)
  div_ $ do
    button_
      [ hxGet_ "/counter/increment",
        hxTarget_ "#counter",
        hxSwap_ "outerHTML",
        class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
      ]
      "inc"
    button_
      [ hxGet_ "/counter/decrement",
        hxTarget_ "#counter",
        hxSwap_ "outerHTML",
        class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
      ]
      "dec"
