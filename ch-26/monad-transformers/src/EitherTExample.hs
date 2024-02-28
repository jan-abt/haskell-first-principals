{-# LANGUAGE OverloadedStrings #-}

module EitherTExample where

import Control.Monad.IO.Class 
import Data.Text.Lazy (Text) 


import Web.Scotty
    ( catch, get, html, queryParam, scotty, ActionM, Parsable )
import Control.Exception (SomeException)
import Web.Scotty.Trans (pathParam)

param' :: Parsable a => Text -> ActionM (Either String a)
param' k =  catch 
              (Right <$> queryParam k)
              (\(_ :: SomeException) -> 
                pure $ Left $ "The key: " ++ show k ++ " was missing!")                

main :: IO ()
main = 
  scotty 3000 $ do 
    get "/:word" $ do
      beam <- pathParam "word"
      m <- param' "1"      
      liftIO $ print (m :: Either String Int)
      liftIO $ print (getValueOrZero m )
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    where 
      getValueOrZero :: Either String Int -> Int
      getValueOrZero = either (const 0) id

-- curl  http://localhost:3000/beam?1=10