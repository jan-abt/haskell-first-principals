module Main where

import qualified URLShortener  as URLShortenerApp
import qualified Database.Redis as DR
import Web.Scotty


main :: IO ()
main = do
  conn <- DR.connect DR.defaultConnectInfo 
  scotty 3000 (URLShortenerApp.routes conn)
