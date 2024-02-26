{-# LANGUAGE OverloadedStrings #-}

module
    URLShortener
        where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C8 
import Data.Text.Encoding (decodeUtf8, encodeUtf8) 
import qualified Data.Text.Lazy as TL
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import qualified Database.Redis as RDB

data AppConfig = AppConfig { 
     host :: String
    ,port :: Int 
    ,conn :: IO RDB.Connection
  }
---------------------------------------------------------------    

routes :: ReaderT AppConfig ScottyM ()
routes = do 
    ioc <- asks conn
    lift $ 
        get "/" $ do
            uri <- queryParam "uri"
            case parseURI (TL.unpack uri) of
                Just _ -> do                        
                    newShortURI <- liftIO mkShortURI
                    let packedShortURI   = C8.pack newShortURI 
                        utf8EncodedURI   = encodeUtf8 (TL.toStrict uri) 
                    serverResponse <- lift $
                                        (ioc >>= (\c -> (saveURI c packedShortURI utf8EncodedURI) ))
                    html (shortURICreatedMessage serverResponse newShortURI)
                Nothing -> text (uriInvalidMessage uri) 
    
    lift $ 
        get "/:short" $ do
            short <- pathParam "short"
            uri  <- lift $ ioc >>= (\c -> (retrieveURI c short))
            case uri of
                Left reply -> text (TL.pack (show reply))
                Right mbBS -> 
                    case mbBS of                     
                        Just bs -> let tbs = TL.fromStrict (decodeUtf8 bs) 
                                in html (uriFoundMessage tbs) 
                        Nothing -> text "uri not found"   
    
---------------------------------------------------------------

mkShortURI :: IO [Char]
mkShortURI = replicateM 7 (randomElement alphaNum)

alphaNum :: String
alphaNum = ['A' .. 'Z'] ++ ['0' .. '9']    

randomElement :: String -> IO Char
randomElement xs = 
     SR.randomRIO withBounds >>= (\idx -> pure (xs !! idx)  ) 
     where
        withBounds =  (0, length xs - 1)

---------------------------------------------------------------

saveURI :: RDB.Connection 
        -> C8.ByteString 
        -> C8.ByteString 
        -> IO (Either RDB.Reply RDB.Status)
saveURI conn shortURI utf8EncodedURI =  RDB.runRedis conn (RDB.set shortURI utf8EncodedURI)

retrieveURI :: RDB.Connection
            -> C8.ByteString   
            -> IO (Either RDB.Reply (Maybe C8.ByteString))
retrieveURI conn shortURI = RDB.runRedis conn (RDB.get shortURI)

---------------------------------------------------------------

makeHtmlLink :: String -> String
makeHtmlLink shortURL =
    concat [
        "<a href=\"", shortURL, "\" target=\"_blank\" >",
            "Click link to retrieve the generated short uri from redis and open it in a new tab",
        "</a>"
    ]

shortURICreatedMessage :: Either RDB.Reply RDB.Status -> String -> TL.Text
shortURICreatedMessage serverResponse shortURI = TL.concat messageParts
    where              -- fmap each String to Lazy Text
        messageParts = TL.pack <$> ["received server response: ", show serverResponse, "<br>",  "generated short URI: ", shortURI, "<br>", makeHtmlLink shortURI]
      
---------------------------------------------------------------

-- because of the added language extension, {-# LANGUAGE OverloadedStrings #-},  String and Text.Lazy are both instances of the IsString.
-- in this function string literals contained in "messageParts"  are implicitly converted to Data.Text.Lazy  because 
-- the type signature indicates that the function takes and returns lazy text .    
uriFoundMessage :: TL.Text -> TL.Text 
uriFoundMessage uri = TL.concat messageParts
    where   
        messageParts = ["URI found: ", "<a href=\"", uri, "\">", uri, "</a>"]        

-- because of the added language extension, {-# LANGUAGE OverloadedStrings #-},  String and Text.Lazy are both instances of the IsString.
-- in this function string literals contained in "messageParts"  are implicitly converted to Data.Text.Lazy  because 
-- the type signature indicates that the function takes and returns lazy text .
uriInvalidMessage :: TL.Text -> TL.Text
uriInvalidMessage uri = TL.concat messageParts
    where   
        messageParts = [uri, " wasn't a url, did you forget http://?" ]
