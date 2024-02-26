module Lib (gimmePerson) where

gimmePerson :: IO ()
gimmePerson  = do
 putStrLn "Enter the person's name"
 name <- getLine
 putStrLn "Enter the person's age"
 age <- getLine
 let person = mkPerson name (read age :: Integer)
 handle person 


type Name = String
type Age = Integer

data Person = 
 Person Name Age 
 deriving Show

data PersonInvalid = 
 NameEmpty
 | AgeTooLow
 | PersonInvalidUnknown String 
 deriving (Eq, Show)
     
mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
 | name /= "" && age > 0 = Right $ Person name age 
 | name == "" = Left NameEmpty
 | age < 0 = Left AgeTooLow
 | otherwise = 
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

handle :: Either PersonInvalid Person -> IO () 
handle (Right p@(Person name age)) = putStrLn ("Person created: " ++ (show p))
handle (Left (PersonInvalidUnknown reason)) = putStrLn ("Failed " ++ (show reason))
handle (Left reason) = putStrLn ("Failed " ++ (show reason))
