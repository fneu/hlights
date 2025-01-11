{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: (Hyperbole :> es) => Eff es (Page '[Message])
page = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

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
