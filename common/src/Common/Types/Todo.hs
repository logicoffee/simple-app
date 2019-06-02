{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Types.Todo where

import           Data.Aeson     (FromJSON, ToJSON, defaultOptions,
                                 genericParseJSON, genericToJSON, parseJSON,
                                 toJSON)
import           Data.Aeson.TH  (fieldLabelModifier)
import           Data.Text.Lazy (Text)
import           Data.Time      (Day, UTCTime (..), getCurrentTime)
import           GHC.Generics   (Generic)

type TodoID = Int

data Todo = Todo
    { id       :: !TodoID
    , task     :: !Text
    , deadline :: !Day
    } deriving (Generic, Eq)

data Todo' = Todo'
    { pTask     :: !Text
    , pDeadline :: !Day
    } deriving (Generic, Eq)

instance ToJSON Todo
instance FromJSON Todo
instance ToJSON Todo' where
    toJSON = genericToJSON jsonOptions
instance FromJSON Todo' where
    parseJSON = genericParseJSON jsonOptions

jsonOptions = defaultOptions
    { fieldLabelModifier = \case
        "pTask"     -> "task"
        "pDeadline" -> "deadline"
    }

makeTodo :: TodoID -> Todo' -> Todo
makeTodo tdId td' = Todo tdId (pTask td') (pDeadline td')

projectTodo' :: Todo -> Todo'
projectTodo' td = Todo' (task td) (deadline td)

defaultTodo' :: IO Todo'
defaultTodo' = do
    UTCTime d _ <- getCurrentTime
    return $ Todo' "default task" d
