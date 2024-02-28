module 
    Doodles 
        where

import Data.List (elemIndex)


meaningOfEverything :: IO Integer
meaningOfEverything = 
    let humbleBeginnings = readIO "1" :: IO Integer
        confusion = fmap (\st -> read ("2" ++ st) :: Integer) (fmap show humbleBeginnings)
    in fmap (*2) confusion

doodles :: IO () 
doodles = do
    what <- meaningOfEverything
    putStr ("It is ")
    print( what )
    return ()
   

-- ==============================================================================

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- ==============================================================================

a :: Maybe Integer
a = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

b :: Maybe Integer
b = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = pure (,) <*> a <*> b

-- ==============================================================================

c :: Maybe Int
c = elemIndex 3 [1, 2, 3, 4, 5] 

d :: Maybe Int
d = elemIndex 4 [1, 2, 3, 4, 5] 

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int 
maxed = (pure max') <*> c <*> d  

-- ==============================================================================

xs = [1, 2, 3]
ys = [4, 5, 6]

e :: Maybe Integer
e = lookup 3 $ zip xs ys 

f :: Maybe Integer
f = lookup 2 $ zip xs ys 

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> e <*> f)
