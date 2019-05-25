{-# LANGUAGE OverloadedStrings #-}
module API where

import           Common.Types.Todo
import           Data.Aeson                    (FromJSON, decode)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.JSString                 as JSS
import           Data.Monoid                   ((<>))
import           JavaScript.Web.XMLHttpRequest
import           Prelude                       hiding (id)


-- TODO: URIは環境変数から取ってくる
baseURL = "http://localhost:3000/todos" :: JSS.JSString
memberURL :: TodoID -> JSS.JSString
memberURL tid = baseURL <> (JSS.pack . show $ tid)

defaultRequest = Request
    { reqMethod = GET
    , reqURI = baseURL
    , reqLogin = Nothing
    , reqHeaders = [("Content-Type", "application/json")]
    , reqWithCredentials = False
    , reqData = NoData
    }

-- TODO: リクエストが失敗した場合の挙動を実装

fetchAllTodos :: IO [Todo]
fetchAllTodos = httpJSON defaultRequest

fetchTodo :: TodoID -> IO Todo
fetchTodo tid = do
    let req = defaultRequest
            { reqURI = memberURL tid
            }
    httpJSON req

createTodo :: Todo' -> IO Todo
createTodo td' = do
    let req = defaultRequest
            { reqMethod = POST
            , reqData = NoData -- 修正
            }
    httpJSON req

updateTodo :: Todo -> IO ()
updateTodo td = do
    let req = defaultRequest
            { reqMethod = PUT
            , reqURI = memberURL $ id td
            , reqData = NoData
            }
    -- このあたりの実装は一旦保留
    httpJSON req :: IO Todo
    return ()

deleteTodo :: TodoID -> IO ()
deleteTodo tid = do
    let req = defaultRequest
            { reqMethod = DELETE
            , reqURI = memberURL tid
            }
    httpJSON req

httpJSON :: FromJSON a => Request -> IO a
httpJSON req = do
    res <- xhrByteString req
    let Just bs = contents res
        Just a  = decode $ LBS.fromStrict bs
    return a
