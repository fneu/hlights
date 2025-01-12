import Data.Text (Text)
import Database.SQLite.Simple
import Lucid
import Lucid.Htmx
import Web.Scotty

-- Database initialization
initializeDB :: FilePath -> IO ()
initializeDB dbFile = do
  conn <- open dbFile
  execute_ conn "CREATE TABLE IF NOT EXISTS Counter (id INTEGER PRIMARY KEY, value INTEGER)"
  -- Insert the initial value if it doesn't exist
  execute_ conn "INSERT OR IGNORE INTO Counter (id, value) VALUES (1, 0)"
  close conn

-- Get the current counter value
getCounter :: Connection -> IO Int
getCounter conn = do
  [Only value] <- query_ conn "SELECT value FROM Counter WHERE id = 1"
  pure value

-- Update the counter value
updateCounter :: Connection -> Int -> IO ()
updateCounter conn newValue = do
  execute conn "UPDATE Counter SET value = ? WHERE id = 1" (Only newValue)

main :: IO ()
main = do
  let dbFile = "hlights.db"
  initializeDB dbFile

  scotty 3000 $ do
    -- Home route
    get "/" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      liftIO $ close conn
      html $ renderText $ counterPage counter

    -- Increment the counter
    get "/increment" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      let newCounter = counter + 1
      liftIO $ updateCounter conn newCounter
      liftIO $ close conn
      html . renderText $
        h1_ [id_ "counter"] (toHtml . show $ newCounter)

    -- Decrement the counter
    get "/decrement" $ do
      conn <- liftIO $ open dbFile
      counter <- liftIO $ getCounter conn
      let newCounter = counter - 1
      liftIO $ updateCounter conn newCounter
      liftIO $ close conn
      html . renderText $
        h1_ [id_ "counter"] (toHtml . show $ newCounter)

    get "/expand-menu" $ html $ renderText hamburgerExpanded
    get "/collapse-menu" $ html $ renderText hamburger

hamburger :: Html ()
hamburger = do
  div_ [class_ "lg:hidden bg-gray-800 text-white flex items-center justify-between p-4 shadow-md"] $ do
    h1_ [class_ "text-xl font-bold"] "Menu"
    button_
      [ class_ "bg-gray-800 text-white focus:outline-none text-2xl p-2 rounded-md",
        hxGet_ "/expand-menu",
        hxTarget_ "#mobile-menu"
      ]
      $ i_ [class_ "fas fa-bars"] "" -- Font Awesome icon for hamburger menu

hamburgerExpanded :: Html ()
hamburgerExpanded = do
  div_ [class_ "lg:hidden bg-gray-800 text-white flex-col p-4 shadow-md"] $ do
    div_ [class_ "flex justify-between items-center"] $ do
      h1_ [class_ "text-xl font-bold"] "Menu"
      button_
        [ class_ "bg-gray-800 text-white focus:outline-none text-2xl p-2 rounded-md",
          hxGet_ "/collapse-menu",
          hxTarget_ "#mobile-menu"
        ]
        $ i_ [class_ "fas fa-times px-1"] "" -- Font Awesome icon for close
    navLinks

navLinks :: Html ()
navLinks = ul_ [class_ "space-y-2"] $ do
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/"] "Home"
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/about"] "About"
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/services"] "Services"
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/contact"] "Contact"

baseLayout :: Html () -> Html ()
baseLayout content = html_ $ do
  head_ $ do
    title_ "Hlights"
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"]
  body_ $ do
    -- Mobile menu
    div_ [id_ "mobile-menu"] $ do
      hamburger
    -- Main container
    div_ [class_ "flex h-screen"] $ do
      -- Sidebar for larger screens
      div_ [class_ "hidden lg:block bg-gray-800 text-white w-64 p-4 shadow-lg"] $ do
        h2_ [class_ "text-2xl font-bold mb-4"] "Menu"
        navLinks
      -- Content area
      div_ [class_ "flex-1 p-4 bg-gray-100"] content

counterPage :: Int -> Html ()
counterPage counter = baseLayout $ do
  h1_ [id_ "counter", class_ "text-3xl font-bold mb-4"] (toHtml . show $ counter)
  div_ $ do
    button_
      [ hxGet_ "/increment",
        hxTarget_ "#counter",
        hxSwap_ "outerHTML",
        class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
      ]
      "inc"
    button_
      [ hxGet_ "/decrement",
        hxTarget_ "#counter",
        hxSwap_ "outerHTML",
        class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 mx-4 rounded"
      ]
      "dec"
