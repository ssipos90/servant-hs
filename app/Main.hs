module Main where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Monad              (void)
import           Database.PostgreSQL.Simple (ConnectInfo (..))
import           GHC.Word                   (Word16)
import           SmallAccounts            (startApp)
import           System.Environment         (getEnv, lookupEnv)

main :: IO ()
main = do
  void $ loadFile defaultConfig
  dbHost <- getEnv "PGHOST"
  dbPort <- maybe 5432 (\s -> read s :: Word16)
                <$> lookupEnv "PGPORT"
  dbName <- getEnv "PGDATABASE"
  dbUser <- getEnv "PGUSER"
  dbPassword <- getEnv "PGPASSWORD"
  _ <- putStrLn "Starting App"
  startApp (ConnectInfo{ connectHost = dbHost
                      , connectDatabase = dbName
                      , connectPort = dbPort
                      , connectUser = dbUser
                      , connectPassword = dbPassword
                      })
