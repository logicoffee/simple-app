{-# LANGUAGE OverloadedStrings #-}
module Web.App where

import           Network.HTTP.Types.Status (status200, status404)
import           Web.Action.Before
import           Web.Action.Todo
import           Web.Scotty

app :: ScottyM ()
app = do
    options "/todos" optionsAction
    options "/todos/:todoId" optionsAction

    get "/todos" fetchAllTodos
    post "/todos" createTodo

    matchAny "/todos/:todoId" beforeAction

    get "/todos/:todoId" fetchTodo
    put "/todos/:todoId" updateTodo
    delete "/todos/:todoId" deleteTodo

    notFound $ status status404

optionsAction :: ActionM ()
optionsAction = do
    addDefaultHeaders
    status status200
