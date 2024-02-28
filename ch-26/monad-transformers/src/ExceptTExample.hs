{-# LANGUAGE OverloadedStrings #-}

module ExceptTExample where


import Control.Monad.IO.Class
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Except 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL 
import Web.Scotty
import Control.Exception (SomeException)

param'' :: Parsable a => Text -> ActionM (Either String a)
param'' k =  catch 
              (Right <$> queryParam k)
              (\(_ :: SomeException) -> 
                pure $ Left $ "The key: " ++ show k ++ " was missing!")                

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = 
  ExceptT $
     catch (Right <$> queryParam k)
           (\(_ :: SomeException) -> pure $ Left $ "The key: " ++ show k ++ " was missing!")                


type Record = (Integer, Integer, Integer, Integer)

tshow :: Record -> Text
tshow = TL.pack . show

main :: IO ()
main = 
  scotty 3000 $ do 
    get "/" $ do
      record <- runExceptT $ do 
        a <- param' "1" 
        liftIO $ print a
        b <- param' "2"
        c <- param' "3"
        d <- param' "4"
        (lift . lift) $ print b 
        return ((a, b, c, d) :: Record)
      case record of
        (Left e) -> text (TL.pack e) 
        (Right r) ->
          html $ mconcat ["<h1>Success! Reco was: ", tshow r, "</h1>"]

-- curl  http://localhost:3000/beam?1=1
-- curl  http://localhost:3000/beam?1=1&2=2&3=3&4=4    