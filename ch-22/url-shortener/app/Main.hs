module Main where

import qualified URLShortenerAlt as URLShortenerApp
import Web.Scotty
import Control.Monad.Trans.Reader 
import URLShortenerAlt (AppConfig(..))
import qualified Database.Redis as RDB

appConfig :: AppConfig  
appConfig =  AppConfig { 
    host = "localhost"
  , port = 3000
  , conn = RDB.connect RDB.defaultConnectInfo  
}  

main :: IO ()
main = 
    let routesAppReader :: ReaderT AppConfig ScottyM () = URLShortenerApp.routes
        withConfig :: AppConfig = appConfig
        app :: ScottyM () = runReaderT routesAppReader withConfig
    
    in scotty (port appConfig) app
      

