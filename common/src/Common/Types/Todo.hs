{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Common.Types.Todo where

import           Data.Aeson         (FromJSON, ToJSON, defaultOptions,
                                     genericParseJSON, genericToJSON, parseJSON,
                                     toJSON)
import           Data.Aeson.TH      (fieldLabelModifier)
import           Data.Text.Lazy     (Text)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)

type TodoID = Int

data Todo = Todo
    { id       :: !TodoID
    , task     :: !Text
    , deadline :: !Day
    } deriving (Generic)

data Todo' = Todo'
    { pTask     :: !Text
    , pDeadline :: !Day
    } deriving (Generic)

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