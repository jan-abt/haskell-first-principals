module
    DPIExample
        where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

data AppConfig = AppConfig
  { dbHost :: String
  , dbPort :: Int
  }

-- Function that uses the configuration
connectToDatabase :: ReaderT AppConfig IO ()
connectToDatabase = do
  config <- ask
  liftIO $ putStrLn $ "Connecting to database at " ++ dbHost config ++ ":" ++ show (dbPort config)

-- Another function using the configuration
performDatabaseQuery :: ReaderT AppConfig IO ()
performDatabaseQuery = do
  config <- ask
  liftIO $ putStrLn $ "Executing database query with config: " ++ show (dbHost config)

-- The main computation combining both functions
mainComputation :: ReaderT AppConfig IO ()
mainComputation = do
  connectToDatabase
  performDatabaseQuery

{-
Once the `ReaderT` computation is run with a specific configuration via `runReaderT`, 
the configuration becomes implicitly available within the entire `ReaderT` context. 
Any function or computation within the `ReaderT` context can access the configuration 
by using the `ask` function.
-}  

main :: IO ()
main = do
  let config = AppConfig { dbHost = "localhost", dbPort = 5432 }
  runReaderT mainComputation config
