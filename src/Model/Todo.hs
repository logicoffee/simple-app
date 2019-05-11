{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Todo where

import           Data.Aeson.Types                     (FromJSON, ToJSON)
import           Data.Text.Lazy                       (Text)
import           Data.Time.Calendar                   (Day)

import           Database.HDBC.Query.TH               (defineTableFromDB',
                                                       makeRelationalRecord)
import           Database.HDBC.Schema.PostgreSQL      (driverPostgreSQL)
import           GHC.Generics                         (Generic)
import           Model.DB                             (connectPG)

import           Data.Functor.ProductIsomorphic.Class ((|$|), (|*|))
import           Database.Relational.Monad.Class      (wheres)
import           Database.Relational.Pi               (Pi)
import           Database.Relational.Projectable      (placeholder, (!), (.=.))
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

findAll :: RType.Query () Todo
findAll = RType.relationalQuery todo

findById :: RType.Query Int Todo
findById = selectTodo

create :: RType.Insert Todo'
create = RType.insert piTodo'

update :: RType.KeyUpdate Int Todo
update = updateTodo

delete :: RType.Delete Int
delete = RType.delete $ \rec -> do
    (ph, _) <- placeholder $ \phRec -> wheres $ rec ! id' .=. phRec
    return ph
