{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Todo where

import           Common.Types.Todo
import           Control.Exception                  (catch)
import           GHC.Generics                       (Generic)
import           Model.DB                           (connectPG)
import           Prelude                            hiding (id)
import           System.IO                          (hPrint, stderr)

import           Database.HDBC                      (SqlError, withTransaction)
import           Database.HDBC.Record               (runDelete, runInsert,
                                                     runKeyUpdate, runQuery')

import           Data.Functor.ProductIsomorphic     ((|$|), (|*|))
import           Database.HDBC.Query.TH             (makeRelationalRecord)
import           Database.Relational.Monad.Class    (wheres)
import           Database.Relational.Pi             (Pi)
import           Database.Relational.Projectable    (value, (!), (.=.))
import           Database.Relational.TH             (defineHasPrimaryKeyInstance,
                                                     definePrimaryQuery,
                                                     definePrimaryUpdate,
                                                     defineTableTypes)
import qualified Database.Relational.Type           as RType
import           Language.Haskell.TH.Name.CamelCase (toVarName)

makeRelationalRecord ''Todo
makeRelationalRecord ''Todo'

defineTableTypes
    (toVarName "tableOfTodo")
    (toVarName "todo")
    (toVarName "insertTodo")
    (toVarName "insertQueryTodo")
    [t|Todo|]
    "todo"
    ["id", "task", "deadline"]

defineHasPrimaryKeyInstance
    [t|Todo|]
    [t|TodoID|]
    [0]

definePrimaryQuery
    (toVarName "selectTodo")
    [t|TodoID|]
    [t|Todo|]
    [|todo|]

definePrimaryUpdate
    (toVarName "updateTodo")
    [t|TodoID|]
    [t|Todo|]
    [|tableOfTodo|]

piTodo' :: Pi Todo Todo'
piTodo' = Todo'
    |$| task'
    |*| deadline'

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
        cnt <- runInsert conn' (RType.derivedInsert piTodo') td'
            `catch` \e -> do
                hPrint stderr (e :: SqlError)
                return 0
            -- TODO: ほんとはcurrval取得して返す
        if cnt == 0
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
