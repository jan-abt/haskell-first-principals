module Main where

import qualified UrlShortener  as UrlShortenerApp
import qualified Database.Redis as DR
import Web.Scotty


main :: IO ()
main = do
  conn <- DR.connect DR.defaultConnectInfo 
  scotty 3000 (UrlShortenerApp.routes conn)
