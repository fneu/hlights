module Main where

import Hlights qualified (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Hlights.someFunc
