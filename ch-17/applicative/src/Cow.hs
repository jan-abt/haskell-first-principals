module
    Cow
        where

import Control.Applicative()

data Cow = Cow {
        name :: String, 
        age :: Int, 
        weight :: Int
    } 
    deriving (Eq, Show)

validateString :: String -> Maybe String 
validateString "" = Nothing
validateString str = Just str

validateNumber :: Int -> Maybe Int 
validateNumber n 
    | n < 0 = Nothing
    | otherwise = Just n        

cowFromString_ :: String -> Int -> Int -> Maybe Cow 
cowFromString_ name' age' weight' =
    case validateString name' of 
        Nothing -> Nothing 
        Just nammy ->
            case validateNumber age' of 
                Nothing -> Nothing 
                Just agey ->
                    case validateNumber weight' of 
                        Nothing -> Nothing
                        Just weighty ->
                            Just (Cow nammy agey weighty)   

cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString name' age' weight' =
    Cow <$> validateString name'
        <*> validateNumber age' 
        <*> validateNumber weight'                            