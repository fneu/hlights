module Pages.Home (homeRoutes, homePage) where

import Control.Monad.Trans.Class (lift)
import Data.Text
import Env (AppM)
import Layout (baseLayout)
import Lucid
import Lucid.Htmx
import Storage (getProperty, updateProperty)
import Web.Scotty.Trans (ScottyT, get, html)

getCounter :: AppM Int
getCounter = do
  prop <- getProperty "counter"
  pure $ maybe 0 read (prop >>= Just . unpack)

updateCounter :: Int -> AppM ()
updateCounter i = updateProperty "counter" (pack . show $ i)

homeRoutes :: ScottyT AppM ()
homeRoutes = do
  get "/home" $ do
    counter <- lift getCounter
    html $ renderText $ homePage counter

  get "/counter/increment" $ do
    counter <- lift getCounter
    let newCounter = counter + 1
    lift $ updateCounter newCounter
    html . renderText $
      h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ newCounter)

  get "/counter/decrement" $ do
    counter <- lift getCounter
    let newCounter = counter - 1
    lift $ updateCounter newCounter
    html . renderText $
      h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ newCounter)

homePage :: Int -> Html ()
homePage counter = baseLayout $ do
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
