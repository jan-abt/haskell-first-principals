{-# LANGUAGE OverloadedStrings #-}

module MaybeTExample where

import Control.Monad.IO.Class 
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Maybe (MaybeT(..)) 
import Data.Text.Lazy (Text) 
import Web.Scotty
import Control.Exception (SomeException)

param' :: Parsable a => Text -> MaybeT ActionM a 
param' k = MaybeT $
    catch (Just <$> queryParam k) (\(_ :: SomeException) -> pure Nothing)

type Record = (Integer, Integer, Integer, Integer)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- pathParam "word"
    record <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      lift $ lift $ putStrLn "..." 
      liftIO $ print d
      return (a, b, c, d) :: MaybeT ActionM Record
    liftIO $ print record
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- http://localhost:3000/beam?1=10&2=20&3=30&4=40    