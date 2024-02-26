module ArbitraryInstances where

import Test.QuickCheck

data Person = Person String Int
  deriving Show

instance Arbitrary Person where
  arbitrary = do
    intVal <- elements [0..99::Int] -- or use "arbitrary" to get any Int 
    strVal <- vectorOf 5 (elements ['A'..'Z'])  -- or use "arbitrary" to get a string mad up of any character 
    return (Person strVal intVal)

-- now you can get a list of sample data of your type
generatePersons :: IO ([Person])
generatePersons = sample' (arbitrary :: Gen Person)


-- makeTuple takes a tuple of generators, inhabited by polimorphicly typed data types.
makeTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
makeTuple = do
 left <- arbitrary
 right <- arbitrary
 return (left, right)

-- makeTuple takes a tuple of generators, which inhabit polimorphic types.
-- We need to specify here the specific typs of generators want.
-- hence: makeTuple:: Gen (Int, String)
-- Otherewise, the generator won't know what type to dispatch to.
generateIntAndStringTuples :: IO ([(Int, String)])
generateIntAndStringTuples = sample' (makeTuple:: Gen (Int, String))

generateIntListAndBoolTuples :: IO ([([Int], Bool)])
generateIntListAndBoolTuples = sample' (makeTuple:: Gen ([Int], Bool))

generate3Ints :: IO ([Int]) 
generate3Ints = sample' $ elements [1, 2, 3::Int]

generateOneBool :: Gen Bool
generateOneBool = choose (False, True)

generateBooleans :: Gen Bool -- increase the probability of being true (sample' generateBooleans)
generateBooleans = elements [False, True, True, True, True]

generateOrderings :: Gen Ordering
generateOrderings = elements [LT, EQ, GT]

generateACharacter :: Gen Char
generateACharacter = elements ['a'..'z']

generateAString :: Gen String
generateAString = vectorOf 10 (elements ['a'..'z'])

makePolimorphicEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b) 
makePolimorphicEither = do
 a <- arbitrary
 b <- arbitrary
 elements [Left a, Right b, Right b] -- twice as likely to generate Right

generateIntStringEithers:: IO ( [Either Int String] )
generateIntStringEithers = sample' ( makePolimorphicEither :: Gen (Either Int String) )

makePolimorphicMaybe:: (Arbitrary a) => Gen (Maybe a)
makePolimorphicMaybe = do
 a <- arbitrary
 elements [Nothing, Just a]

generateIntMaybes:: IO ([Maybe Int])
generateIntMaybes = sample' (makePolimorphicMaybe::Gen (Maybe Int))

