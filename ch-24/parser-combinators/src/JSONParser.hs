{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module
    JSONParser
        where

import Control.Applicative
import Data.Aeson ( decode, (.:), FromJSON(parseJSON), Value(Object) )
-- import qualified Data.ByteString as SBS -- strict
import qualified Data.ByteString.Lazy as LZ
import Text.RawString.QQ

jsonString :: LZ.ByteString
jsonString = [r|
{ 
  "section":  {"host": "wikipedia.org"},
  "what": {"red": "intoothandclaw"} 
}
|]

data DomainObject = 
    DomainObject {
        section :: Host,
        what :: Color
    } deriving (Eq, Show)

newtype Host =
    Host String deriving (Eq, Show)

type Annotation = String

data Color =
    Red Annotation
    | Blue Annotation
    | Yellow Annotation deriving (Eq, Show)

-- parse JSON --

-- pick up the values of the json keys "section" and "what" and 
-- apply the DomainObject data type to them
instance FromJSON DomainObject 
    where 
        parseJSON (Object v) =
            DomainObject <$> v .: "section" 
                     <*> v .: "what"
        parseJSON _ = 
            fail "Expected an object for DomainObject"

-- pick up the respective value of the "host"  json key and 
-- apply the Host data type to it
instance FromJSON Host 
    where 
        parseJSON (Object v) = 
            Host <$> v .: "host"
        parseJSON _ =
            fail "Expected an object for Host"

-- pick up the  "annotation" value for a
-- json key, either  "red", "blue" or "yellow",
-- and apply our Color enum to both
instance FromJSON Color 
    where 
        parseJSON (Object v) =
                (Red <$> v .: "red")
            <|> (Blue <$> v .: "blue")
            <|> (Yellow <$> v .: "yellow")
        parseJSON _ = 
            fail "Expected an object for Color"    

main :: IO ()
main = do

    putStrLn "------------- DECODE TO AESON VALUE data type ----------------"
    let blahL :: Maybe Value
        blahL = decode jsonString
    print blahL
    putStrLn "------------- DECODE TO DomainObject data type --------------"
    let d = decode jsonString :: Maybe DomainObject 
    print d