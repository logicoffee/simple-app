module Main where

import           Web.App
import           Web.Scotty

main :: IO ()
main = scotty 3000 app
