{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Todo where

import           Data.Aeson.Types                (FromJSON, ToJSON)
import           Data.Text.Lazy                  (Text)
import           Data.Time.Calendar              (Day)

import           Database.HDBC.Query.TH          (defineTableFromDB',
                                                  makeRelationalRecord)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           GHC.Generics                    (Generic)
import           Model.DB                        (connectPG)

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

data Todo' = Todo'
    { pTask     :: !Text
    , pDeadline :: !Day
    } deriving (Generic)

instance ToJSON Todo
instance FromJSON Todo'
