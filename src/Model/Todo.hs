{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Todo where

import           Control.Exception                    (catch)
import           Data.Aeson.Types                     (FromJSON, ToJSON)
import           Data.Functor.ProductIsomorphic.Class ((|$|), (|*|))
import           Data.Text.Lazy                       (Text)
import           Data.Time.Calendar                   (Day)
import           GHC.Generics                         (Generic)
import           Model.DB                             (connectPG)
import           Prelude                              hiding (id)
import           System.IO                            (hPrint, stderr)

import           Database.HDBC                        (SqlError,
                                                       withTransaction)
import           Database.HDBC.Query.TH               (defineTableFromDB',
                                                       makeRelationalRecord)
import           Database.HDBC.Record                 (runDelete, runInsert,
                                                       runKeyUpdate, runQuery')
import           Database.HDBC.Schema.PostgreSQL      (driverPostgreSQL)

import           Database.Relational.Monad.Class      (wheres)
import           Database.Relational.Pi               (Pi)
import           Database.Relational.Projectable      (placeholder, value, (!),
                                                       (.=.))
import qualified Database.Relational.Type             as RType

defineTableFromDB'
    connectPG
    driverPostgreSQL
    "public"
    "todo"
    [ ("id", [t|Int|])
    , ("task", [t|Text|])
    , ("deadline", [t|Day|])
    ]
    [''Show, ''Generic]

type TodoID = Int

data Todo' = Todo'
    { pTask     :: !Text
    , pDeadline :: !Day
    } deriving (Generic)

makeRelationalRecord ''Todo'

piTodo' :: Pi Todo Todo'
piTodo' = Todo'
    |$| task'
    |*| deadline'

instance ToJSON Todo
instance FromJSON Todo'

makeTodo :: TodoID -> Todo' -> Todo
makeTodo tdId td' = Todo tdId (pTask td') (pDeadline td')

fetchAll :: IO [Todo]
fetchAll = do
    conn <- connectPG
    withTransaction conn $ \conn' ->
        runQuery' conn' (RType.relationalQuery todo) ()
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return []

fetch :: TodoID -> IO (Maybe Todo)
fetch todoId = do
    conn <- connectPG
    todos <- withTransaction conn $ \conn' ->
        runQuery' conn' selectTodo todoId
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return []
    if null todos
        then return Nothing
        else return $ Just $ head todos

create :: Todo' -> IO TodoID
create td' = do
    conn <- connectPG
    withTransaction conn $ \conn' -> do
        cnt <- runInsert conn' (RType.insert piTodo') td'
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return 0
        if cnt == 1
            -- TODO: ほんとはcurrval取得して返す
            then return 0
            else return 1

update :: Todo -> IO Integer
update td = do
    conn <- connectPG
    withTransaction conn $ \conn' ->
        runKeyUpdate conn' updateTodo td
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return 0

delete :: TodoID -> IO Integer
delete todoId = do
    conn <- connectPG
    withTransaction conn $ \conn' ->
        runDelete conn' (deleteTodoSQL todoId) ()
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return 0

deleteTodoSQL :: Int -> RType.Delete ()
deleteTodoSQL todoId = RType.deleteNoPH $ \rec ->
    wheres $ rec ! id' .=. value todoId
