{-# LANGUAGE OverloadedStrings #-}
module API where

import           Common.Types.Todo
import           Data.Aeson                    (FromJSON, ToJSON, decode,
                                                encode)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.JSString                 as JSS
import           Data.Monoid                   ((<>))
import           JavaScript.Web.XMLHttpRequest
import           Miso
import           Prelude                       hiding (id)


-- TODO: URIは環境変数から取ってくる
baseURL = "http://localhost:3000/todos" :: JSS.JSString
memberURL :: TodoID -> JSS.JSString
memberURL tid = baseURL <> (JSS.pack . show $ tid)

makeRequest :: Method -> JSS.JSString -> RequestData -> Request
makeRequest mth uri dt = Request
    { reqMethod = mth
    , reqURI = uri
    , reqLogin = Nothing
    , reqHeaders =
        [ ("Content-Type", "application/json") ]
    , reqWithCredentials = False
    , reqData = dt
    }

-- TODO: リクエストが失敗した場合の挙動を実装

fetchAllTodos :: IO [Todo]
fetchAllTodos = do
    mbTds <-  httpJSON $ makeRequest GET baseURL NoData
    case mbTds of
        Nothing  -> return []
        Just tds -> return tds

fetchTodo :: TodoID -> IO (Maybe Todo)
fetchTodo tid = httpJSON $ makeRequest GET (memberURL tid) NoData

createTodo :: Todo' -> IO (Maybe Todo)
createTodo td' = do
    jsonData <- stringify td'
    httpJSON $ makeRequest POST baseURL $ StringData jsonData

updateTodo :: Todo -> IO ()
updateTodo td = do
    jsonData <- stringify td
    let req = makeRequest
            PUT
            (memberURL $ id td)
            (StringData jsonData)
    -- このあたりの実装は一旦保留
    httpJSON req :: IO (Maybe Todo)
    return ()

deleteTodo :: TodoID -> IO ()
deleteTodo tid = do
    httpJSON $ makeRequest DELETE (memberURL tid) NoData :: IO (Maybe ())
    return ()

httpJSON :: FromJSON a => Request -> IO (Maybe a)
httpJSON req = do
    res <- xhrByteString req
    return $ contents res >>= (decode . LBS.fromStrict)
