module 
    Person 
        where

newtype Name = -- even though Name, Address are strings, they are distinct types
    Name String 
    deriving (Eq, Show) 

newtype Address = -- even though Name, Address are strings, they are distinct types
    Address String 
    deriving (Eq, Show)

data Person =
    Person Name Address -- therefore you cannot switch the argument order such as in, for example: Address, Name
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String 
validateLength maxLen s =
    if (length s) > maxLen 
    then Nothing
    else Just s

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s            

mkPerson_ :: String -> String -> Maybe Person 
mkPerson_ n a =
    case mkName n of 
        Nothing -> Nothing 
        Just n' ->
            case mkAddress a of 
                Nothing -> Nothing 
                Just a' -> Just $ Person n' a'


mkPerson :: String -> String -> Maybe Person 
mkPerson n a =
    Person <$> mkName n <*> mkAddress a                

 -- mkPerson "Jan" "SteubenStraße 9" || Maybe Person
 -- OR
 -- let maybeName = (mkName "Jan") || Maybe String
 -- let maybeAddress = (mkAddress "SteubenStraße 9") || Maybe String
 -- Person <$> maybeName <*> maybeAddress  || Maybe Person
 -- OR
 -- let partiallyAppliedPerson = Person <$> maybeName || Maybe (Address -> Person)
 -- partiallyAppliedPerson <*> maybeAddress || Maybe Person