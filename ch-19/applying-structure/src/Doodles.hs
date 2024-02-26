{-# LANGUAGE OverloadedStrings #-}

module 
    Doodles 
        where

import Web.Scotty
import Data.Time.Clock
import Data.Text (Text) 
import qualified Data.Text as T
import qualified Data.UUID as U 
import qualified Data.UUID.V4 as V4
import Network.Socket
import Codec.Binary.UTF8.String


doodles :: IO () 
doodles = do
    what <- life'sMeaning
    putStr ("It is ")
    print( what )
    return ()

life'sMeaning :: IO Integer
life'sMeaning = 
    humbleBeginnings >>= stormAndStress >>= enlightenment

 where 
    humbleBeginnings :: IO Integer
    humbleBeginnings = readIO "0" 

    stormAndStress :: Integer -> IO Integer
    stormAndStress i = let storm = readIO $ show i :: IO Integer
                           stress = (\x -> readIO ( "2" ++ show x )   )
                       in storm >>= stress

    enlightenment :: Integer -> IO Integer                  
    enlightenment = (\i -> readIO $ show (2 * i + 2))

main ::  IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]    
   
{-
partially applying addUTCTime to the offset we’re going to add to the second argument, 
then mapping it over the IO action that gets us the current time:
-}   
offsetCurrentTime :: NominalDiffTime -> IO UTCTime 
offsetCurrentTime daysOffset = 
    fmap (addUTCTime (daysOffset * 24 * 3600)) $ getCurrentTime   

textUuid :: IO Text
textUuid = fmap (T.pack . U.toString) V4.nextRandom    

openSocket :: FilePath -> IO Socket 
openSocket p =  do
    sock <- socket AF_UNIX Stream defaultProtocol 
    connect sock sockAddr
    return sock
    where 
        sockAddr = SockAddrUnix . encodeString $ p

closeSocket :: Socket -> IO ()
closeSocket = close
