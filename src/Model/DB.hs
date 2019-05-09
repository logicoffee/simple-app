module Model.DB where

import           Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import           System.Environment       (getEnv)
import           Text.Printf              (printf)

connectPG :: IO Connection
connectPG = do
    host     <- getEnv "PG_HOST"
    port     <- getEnv "PG_PORT"
    user     <- getEnv "PG_USER"
    db_name  <- getEnv "PG_DB_NAME"
    password <- getEnv "PG_PASSWORD"
    connectPostgreSQL $ printf
        "host=%s port=%s user= %s dbname=%s password=%s sslmode=disable"
        host
        port
        user
        db_name
        password
