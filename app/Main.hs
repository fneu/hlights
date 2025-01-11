{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Effectful
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Hlights") (routeRequest router)
  where
    router :: (Hyperbole :> es) => AppRoute -> Eff es Response
    router Home = runPage page
    router About = runPage page2

data AppRoute = Home | About deriving (Eq, Generic, Show)

instance Route AppRoute where
  baseRoute = Just Home

page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

page2 :: (Hyperbole :> es) => Eff es (Page '[Message])
page2 = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello2"
    hyper Message2 $ messageView "World2"

data Message = Message1 | Message2
  deriving (Show, Read, ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView msg = do
  row id $ do
    button (Louder msg) id "Louder"
    el_ $ text msg
