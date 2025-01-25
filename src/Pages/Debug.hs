module Pages.Debug (debugRoutes, debugPage) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks)
import Data.Map qualified as M
import Data.Text (Text)
import Dirigera.Devices
import Env (AppM, Env (..))
import Layout (baseLayout)
import Lucid
import Web.Scotty.Trans (ScottyT, get, html)

debugRoutes :: ScottyT AppM ()
debugRoutes = do
  get "/debug" $ do
    envLights <- lift $ asks (.lights)
    lights <- liftIO $ readTVarIO envLights
    html $ renderText $ debugPage lights

debugPage :: M.Map Text Device -> Html ()
debugPage lights = baseLayout $ do
  forM_ (M.toList lights) $ \(lightId, light) -> do
    div_ [class_ "p-4 border-b border-gray-200"] $ do
      h2_ [class_ "text-xl font-bold"] $ toHtml $ show lightId
      p_ $ toHtml $ show light
