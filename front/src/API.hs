{-# LANGUAGE OverloadedStrings #-}
module API where

import           Common.Types.Todo
import           Data.Aeson            (encode)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Monoid           ((<>))
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Prelude               hiding (id)

defaultHost = "localhost" :: ByteString
defaultPort = 3000        :: Int
baseURL     = "/todos"    :: ByteString
memberURL :: TodoID -> ByteString
memberURL tid = baseURL <> (pack . show $ tid)

initReq = defaultRequest
    { host = defaultHost
    , port = defaultPort
    }

-- TODO: リクエストが失敗した場合の挙動を実装

fetchAllTodos :: IO [Todo]
fetchAllTodos = do
    let req = initReq
            { method = methodGet
            , path = baseURL
            }
    res <- httpJSON req :: IO (Response [Todo])
    return $ getResponseBody res

fetchTodo :: TodoID -> IO Todo
fetchTodo tid = do
    let req = initReq
            { method = methodGet
            , path = memberURL tid
            }
    res <- httpJSON req :: IO (Response Todo)
    return $ getResponseBody res

createTodo :: Todo' -> IO Todo
createTodo td' = do
    let req = initReq
            { method = methodPost
            , path = baseURL
            , requestBody = RequestBodyLBS $ encode td'
            , requestHeaders = [(hContentType, "application/json")]
            }
    res <- httpJSON req :: IO (Response TodoID)
    return $ makeTodo (getResponseBody res) td'

updateTodo :: Todo -> IO ()
updateTodo td = do
    let req = initReq
            { method = methodPut
            , path = memberURL $ id td
            , requestBody = RequestBodyLBS $ encode td
            , requestHeaders = [(hContentType, "application/json")]
            }
    -- このあたりの実装は一旦保留
    httpJSON req :: IO (Response Int)
    return ()

deleteTodo :: TodoID -> IO ()
deleteTodo tid = do
    let req = initReq
            { method = methodDelete
            , path = memberURL tid
            }
    httpJSON req :: IO (Response Integer)
    return ()
