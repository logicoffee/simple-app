{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Common.Types.Todo
import           Effect
import           Miso
import           View

main :: IO ()
main = startApp App
    { initialAction = Init
    , model = Model [] $ Todo' "" $ read "2020-01-01"
    , update = updateModel
    , view = render
    , events = defaultEvents
    , subs = []
    , mountPoint = Nothing
    }
