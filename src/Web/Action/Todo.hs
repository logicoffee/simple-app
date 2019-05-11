{-# LANGUAGE OverloadedStrings #-}
module Web.Action.Todo where

import           Control.Monad             (when)
import           Control.Monad.IO.Class    (liftIO)
import           Database.HDBC.Record      (runDelete, runInsert, runKeyUpdate,
                                            runQuery')
import           Model.DB                  (connectPG)
import           Model.Todo
import           Network.HTTP.Types.Status (status200, status404, status500)
import           Web.Scotty

fetchAllTodos :: ActionM ()
fetchAllTodos = do
    conn <- liftIO connectPG
    todos <- liftIO $ runQuery' conn findAll ()
    json todos

fetchTodo :: ActionM ()
fetchTodo = do
    todoId <- param "todoId"
    conn   <- liftIO connectPG
    todos  <- liftIO $ runQuery' conn findById todoId
    case todos of
        []  -> status status404
        t:_ -> json t

createTodo :: ActionM ()
createTodo = do
    td'  <- jsonData :: ActionM Todo'
    conn <- liftIO connectPG
    cnt  <- liftIO $ runInsert conn create td'
    case cnt of
        1 -> status status200
        _ -> status status500

updateTodo :: ActionM ()
updateTodo = do
    conn   <- liftIO connectPG
    todoId <- param "todoId"
    todos  <- liftIO $ runQuery' conn findById todoId
    when (null todos) $ do
        status status404
        finish
    td'   <- jsonData :: ActionM Todo'
    let td = Todo todoId (pTask td') (pDeadline td')
    cnt  <- liftIO $ runKeyUpdate conn update td
    case cnt of
        1 -> status status200
        _ -> status status500

deleteTodo :: ActionM ()
deleteTodo = do
    conn   <- liftIO connectPG
    todoId <- param "todoId"
    todos  <- liftIO $ runQuery' conn findById todoId
    when (null todos) $ do
        status status404
        finish
    cnt <- liftIO $ runDelete conn Model.Todo.delete todoId
    case cnt of
        1 -> status status200
        _ -> status status500
