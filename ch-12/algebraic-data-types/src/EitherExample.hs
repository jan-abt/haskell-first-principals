module EitherExample where

type Name = String
type Age = Integer

data Person = 
 Person Name Age 
 deriving Show

type Validator a = Either [Reason] a

data Reason = 
 NameEmpty 
 | AgeTooLow 
 deriving (Eq, Show)

ageOkay :: Age -> Either Reason Age 
ageOkay age = 
 case age >= 0 of
  True -> Right age
  False -> Left AgeTooLow

nameOkay :: Name -> Either Reason Name 
nameOkay name = 
 case name /= "" of
  True -> Right name
  False -> Left NameEmpty

mkPerson :: Name -> Age -> Either Reason Person 
mkPerson name age
 | name /= "" && age >= 0 = Right $ Person name age
 | name == "" = Left NameEmpty
 | otherwise = Left AgeTooLow

mkPerson' :: Name -> Age -> Validator Person
mkPerson' n a = validate (nameOkay n) (ageOkay a)
 where 
  validate (Left n) (Right _) = Left [n]
  validate (Right _) (Left a)  = Left [a]
  validate (Left n) (Left a) = Left [n, a]
  validate (Right n) (Right a) = Right (Person n a) 
