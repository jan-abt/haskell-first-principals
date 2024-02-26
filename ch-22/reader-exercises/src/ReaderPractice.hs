module
    ReaderPractice
        where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


getForKey :: Eq a => a -> [(a, b)] -> Maybe b
getForKey _ [] = Nothing
getForKey k (p:ps) = case (fst p == k) of
                 True -> Just $ snd p
                 _    -> getForKey k ps

-- zip x and y using 3 as the lookup key
x3y :: Maybe Integer 
x3y = getForKey 3 $ zip x y

-- zip y and z using 6 as the lookup key
y6z :: Maybe Integer 
y6z = getForKey 6 $ zip y z

-- zip x and y using 4 as the lookup key 
x4y :: Maybe Integer
x4y = getForKey 4 $ zip x y -- will return Nothing

-- now zip x and z using a variable lookup key
xnz :: Integer -> Maybe Integer 
xnz n = getForKey n $ zip x z        

x1 :: Maybe (Integer, Integer)
x1 = pure (,) <*> x3y <*> y6z    

x2 :: Maybe (Integer, Integer)
x2 = pure (,) <*> x4y <*> y6z    

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = liftA2 (,)  xnz  xnz  $ n

summed :: Num c => (c, c) -> c 
summed (c,c') = uncurry (+) (c,c')

bolt :: Integer -> Bool
bolt = pure (&&) <*> (>3) <*> (<8)

seqA :: Integral a => a -> [Bool]
seqA v = sequenceA [(>3), (<8), even] v    

s' :: Maybe Integer    
s' = summed <$> ((,) <$> x3y <*> y6z)    


main :: IO () 
main = do
    print $ sequenceA [Just 3, Just 2, Just 1] 
    print $ sequenceA [x, y]
    print $ sequenceA [x3y, y6z]
    print $ summed <$> ((,) <$> x3y <*> y6z) 
    print $ fmap summed ((,) <$> x3y <*> x4y) 
    print $ bolt 7
    print $ fmap bolt z    
    print $ foldr (\e a ->  e && a) True (seqA 7)
    print $ seqA $ fromMaybe 0 s'
    print $  bolt $ fromMaybe 0 y6z
    print $  bolt $ fromMaybe 0 (xnz 1)
    

