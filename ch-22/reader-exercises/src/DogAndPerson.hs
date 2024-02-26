module
    DogAndPerson
        where

import Reader' 
        
newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)                  

data Person = Person {
        humanName :: HumanName, 
        dogName :: DogName, 
        address :: Address
    } 
    deriving (Eq, Show)

data Dog = Dog {
        dogsName :: DogName, 
        dogsAddress :: Address 
    } 
    deriving (Eq, Show)

bigBird :: Person
bigBird =
    Person (HumanName "Big Bird") 
           (DogName "Barkley") 
           (Address "Sesame Street")

chris :: Person
chris = 
    Person (HumanName "Chris Allen")
           (DogName "Papu") 
           (Address "Austin")

-----------------------------------------------------------------------------------------------
 --------------------------------  APPLYING THE DATA TYPES -----------------------------------
-----------------------------------------------------------------------------------------------

-- without Reader
getDog :: Person -> Dog 
getDog p = Dog (dogName p) (address p)    
--  getDog chris

-- with Reader, basicallly a two arg func is applied to the 1st applicative, 
-- then the result of that, being a one arg function, is applied to the 2nd applicative.
getDogApplicatively:: Person -> Dog
getDogApplicatively =  Dog <$> dogName <*> address    
-- getDogApplicatively chris

getDogA2Lifted :: Person -> Dog
getDogA2Lifted =  innerLiftA2 Dog dogName address   
-- getDogA2Lifted chris

innerLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
innerLiftA2 f a1 a2 = pure f <*> a1 <*> a2

getDogWithDo :: Person -> Dog
getDogWithDo = do
    name <- dogName
    addr <- address
    return $ Dog name addr
-- getDogWithDo chris   

getDogApplicativelyWithReader :: Reader' Person Dog
-- getDogApplicativelyWithReader =  Reader' (\p -> getDog p ) :: Reader' Person Dog
-- getDogApplicativelyWithReader =  let rf = pure getDog :: Reader' Person (Person -> Dog)
--                                  in rf <*> Reader' (\p -> p)
getDogApplicativelyWithReader =  pure getDog <*> Reader' (\p -> p) 
--  runReader' getDogApplicativelyWithReader  chris

getDogMonadiclyWithReader :: Reader' Person Dog
getDogMonadiclyWithReader = return getDog >>= (\pd -> Reader' pd)
--  runReader' getDogMonadiclyWithReader  chris