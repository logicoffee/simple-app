{-# LANGUAGE OverloadedStrings #-}
module Web.Action.Before where

import           Control.Monad.IO.Class    (liftIO)
import           Model.Todo
import           Network.HTTP.Types.Status (status404)
import           Web.Scotty

beforeAction :: ActionM ()
beforeAction = do
    todoId <- param "todoId"
    maybeTodo <- liftIO $ fetch todoId
    case maybeTodo of
        Nothing -> status status404 >> finish
        _       -> next
