module Layout (layoutRoutes, baseLayout) where

import Data.Text (Text)
import Env (AppM)
import Lucid
import Lucid.Htmx
import Web.Scotty.Trans (ScottyT, file, get, html, setHeader)

layoutRoutes :: ScottyT AppM ()
layoutRoutes = do
  get "/favicon.svg" $ do
    setHeader "Content-Type" "image/svg+xml"
    file "static/favicon.svg"
  get "/htmx" $ do
    setHeader "Content-Type" "application/javascript"
    file "static/htmx.min.js"
  get "/htmx-ext-sse" $ do
    setHeader "Content-Type" "application/javascript"
    file "static/htmx-ext-sse.min.js"
  get "/expand-menu" $ html $ renderText hamburgerExpanded
  get "/collapse-menu" $ html $ renderText hamburger

navLinks :: Html ()
navLinks = ul_ [class_ "space-y-2"] $ do
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/home"] "Home"
  li_ [class_ "hover:bg-gray-400 p-2 rounded"] $ a_ [href_ "/connection"] "Connection"

hamburger :: Html ()
hamburger = do
  div_ [class_ "fixed top-0 left-0 right-0 lg:hidden bg-gray-800 text-white flex items-center justify-between p-4 shadow-md z-50"] $ do
    h1_ [class_ "text-xl font-bold"] "Menu"
    button_
      [ class_ "bg-gray-800 text-white focus:outline-none text-2xl p-2 rounded-md",
        hxGet_ "/expand-menu",
        hxTarget_ "#mobile-menu"
      ]
      $ i_ [class_ "fas fa-bars"] "" -- Font Awesome icon for hamburger menu

hamburgerExpanded :: Html ()
hamburgerExpanded = do
  div_ [class_ "fixed top-0 left-0 right-0 lg:hidden bg-gray-800 text-white flex-col p-4 shadow-md z-50"] $ do
    div_ [class_ "flex justify-between items-center"] $ do
      h1_ [class_ "text-xl font-bold"] "Menu"
      button_
        [ class_ "bg-gray-800 text-white focus:outline-none text-2xl p-2 rounded-md",
          hxGet_ "/collapse-menu",
          hxTarget_ "#mobile-menu"
        ]
        $ i_ [class_ "fas fa-times px-1"] "" -- Font Awesome icon for close
    navLinks

baseLayout :: Html () -> Html ()
baseLayout content = html_ $ do
  head_ $ do
    title_ "Hlights"
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
    script_ [src_ "/htmx", crossorigin_ "anonymous"] ("" :: Text)
    script_ [src_ "/htmx-ext-sse", crossorigin_ "anonymous"] ("" :: Text)
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"]
  body_ [class_ "bg-gray-100"] $ do
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
      div_ [class_ "flex-1 mt-2 lg:mt-0"] $ do
        div_ [class_ "w-full p-4 bg-gray-100 mt-16 lg:mt-0"] content
