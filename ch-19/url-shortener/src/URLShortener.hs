{-# LANGUAGE OverloadedStrings #-}

module
    UrlShortener
        where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C8 
import Data.Text.Encoding (decodeUtf8, encodeUtf8) 
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as DB
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

---------------------------------------------------------------    

routes :: DB.Connection -> ScottyM ()
routes conn = do 
    -- generate a short uri for the full-length uri that was provided as a query parameter and store the key value pair in redis 
    get "/" $ do 
        uri <- queryParam "uri" -- "uri" query parameter (name1=value1&name2= ...)
        case parseURI (TL.unpack uri) of
            Just _ -> do
                newShortURI <- liftIO mkShortURI
                let packedShortURI   = C8.pack newShortURI 
                    utf8EncodedURI   = encodeUtf8 (TL.toStrict uri) 
                serverResponse <- liftIO (saveURI conn packedShortURI utf8EncodedURI) 
                html (shortURICreatedMessage serverResponse newShortURI)
            Nothing -> text (uriInvalidMessage uri) 
    -- retrieve from redis the full-length uri for the short uri that was provided in the sub-path
    get "/:short" $ do
        short <- queryParam "short"  -- URI path capture (/users/:name/)
        uri <- liftIO (retrieveURI conn short) 
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

saveURI :: DB.Connection 
        -> C8.ByteString 
        -> C8.ByteString 
        -> IO (Either DB.Reply DB.Status)
saveURI conn shortURI utf8EncodedURI =  DB.runRedis conn (DB.set shortURI utf8EncodedURI)

retrieveURI :: DB.Connection
            -> C8.ByteString   
            -> IO (Either DB.Reply (Maybe C8.ByteString))
retrieveURI conn shortURI = DB.runRedis conn (DB.get shortURI)

---------------------------------------------------------------

makeHtmlLink :: String -> String
makeHtmlLink shortURL =
    concat [
        "<a href=\"", shortURL, "\" target=\"_blank\" >",
            "Click link to retrieve the generated short uri from redis and open it in a new tab",
        "</a>"
    ]

shortURICreatedMessage :: Either DB.Reply DB.Status -> String -> TL.Text
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
