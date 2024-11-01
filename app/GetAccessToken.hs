module Main where

import Auth qualified
import Control.Concurrent
import Data.Text (pack)
import System.Environment (getEnv)
import System.Exit (exitFailure)

main :: IO ()
main = do
  ip <- getEnv "DIRIGERA_IP"
  codeVerifier <- Auth.generateCodeVerifier
  maybeCode <- Auth.requestAuthCode (pack ip) codeVerifier
  case maybeCode of
    Nothing -> exitFailure
    Just code -> do
      putStrLn "Press Action Button on Dirigera within 1 minute"
      threadDelay $ 59 * 1000 * 1000
      maybeToken <- Auth.requestAccessToken (pack ip) codeVerifier code
      maybe exitFailure print maybeToken
