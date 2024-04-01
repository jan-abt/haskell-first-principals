module StackemHigh where


import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow ((&&&))

-- configuration data type
data Config = Config {
    name :: String
  , version :: String
}

-- application state new type
newtype ScoreBoard = ScoreBoard {
  score :: Int
}

-- Combined Monad transformer stack
type MyApp = StateT ScoreBoard (ReaderT Config IO)

{- |
   We have a monad transformer stack `MyApp`, 
   which is defined as `ReaderT` `Config` `IO`, 
   meaning it's a reader monad transformer over the IO monad. 
   When we want to perform IO actions within this stack, 
   we need to lift those actions using `liftIO` to bring them 
   into the `ReaderT` `Config` `IO` context.
 -}
writeToTerminal :: String -> MyApp ()
writeToTerminal msg = liftIO $ putStrLn msg

-- Example appfunction using the transformer stack
myAppFunction :: MyApp ()
myAppFunction = do
  (appName, appVersion) <-  asks $ name &&& version
  initialScore <- gets score
  -- business logic here, using config and state
  writeToTerminal 
    $ "Hello from "
    <> appName
    <> ", v." 
    <> appVersion
    <> ".\nYour initial score is " 
    <> show initialScore
    <> "."

-- Run the app, which is a transformer stack,applying initial state and configuration
runWithStateAndConfig :: MyApp a -> ScoreBoard -> Config -> IO a
runWithStateAndConfig app stt = -- config arg is implied, due to Eta reduce 
  runReaderT (evalStateT app stt) 

-- Example usage
main :: IO ()
main = do
  let initialConfig = -- initialize configuration
        Config { name = "Super App", version = "1.0"} 
      initialState = -- initialize state 
        ScoreBoard { score = 0 } 

  runWithStateAndConfig
    myAppFunction
    initialState 
    initialConfig 
