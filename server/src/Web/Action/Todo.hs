{-# LANGUAGE OverloadedStrings #-}
module Web.Action.Todo where

import           Common.Types.Todo
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (liftIO)
import           Model.Todo
import           Network.HTTP.Types.Status (status200, status404, status500)
import           Web.Scotty

fetchAllTodos :: ActionM ()
fetchAllTodos = do
    addDefaultHeaders
    todos <- liftIO fetchAll
    json todos

fetchTodo :: ActionM ()
fetchTodo = do
    addDefaultHeaders
    todoId    <- param "todoId"
    maybeTodo <- liftIO $ fetch todoId
    case maybeTodo of
        Nothing -> status status404
        Just td -> json td

createTodo :: ActionM ()
createTodo = do
    addDefaultHeaders
    td'  <- jsonData :: ActionM Todo'
    tdId <- liftIO $ create td'
    if tdId == 0
        then status status500
        else json $ makeTodo tdId td'

updateTodo :: ActionM ()
updateTodo = do
    addDefaultHeaders
    td <- makeTodo <$> param "todoId" <*> jsonData
    cnt <- liftIO $ update td
    case cnt of
        0 -> status status500
        _ -> json td

deleteTodo :: ActionM ()
deleteTodo = do
    addDefaultHeaders
    todoId <- param "todoId"
    cnt    <- liftIO $ Model.Todo.delete todoId
    case cnt of
        -- TODO: 削除成功時にはjsonを返したい
        0 -> status status500
        _ -> status status200

addDefaultHeaders :: ActionM ()
addDefaultHeaders = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
    addHeader "Access-Control-Allow-Headers" "Content-Type"
    addHeader "Content-Type" "application/json"
