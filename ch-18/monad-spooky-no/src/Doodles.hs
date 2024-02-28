module Doodles where

import Control.Monad (ap, join, liftM2)

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
                           stress = (\value -> readIO ( "2" ++ show value )   )
                       in storm >>= stress

    enlightenment :: Integer -> IO Integer                  
    enlightenment = (\i -> readIO $ show (2 * i + 2))

--    
-- variations on bind operations     
--

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b 
bindMaybe Nothing _ = Nothing
bindMaybe (Just v) f =  f v
-- bindMaybe (Just 4) (\n -> Just (n+4))    

bindStar :: Applicative a => a x -> a (x -> y) -> a y
bindStar a f =  f <*> a 
-- bindStar (Just 4) (Just (\n -> n+4))
    
bindAp :: Monad m => m x -> m (x -> y) -> m y
bindAp a f = ap f a
-- bindAp (Just 4) (Just (\n -> n+4))

bindF :: Functor f => f x -> (x -> y) -> f y
bindF t f = fmap f t 
-- bindF (Just 4) (\n -> Just (n+4))

bindM :: Monad m => m x -> (x -> m y) -> m y
bindM m f = join $ fmap f m 
-- bindM (Just 4) (\n -> Just (n+4))    

-- simple int operation that returns itself and the value 1, 
-- wrapped into a monadic structure, here a list
andOne :: Int -> [Int]
andOne x = [x, 1]    

-- fmap andOne [4, 5, 6]      
-- concat $ fmap andOne [4, 5, 6]    
-- [1,2,3] >>= andOne 

functorialFmap :: Functor f => (a -> b) -> f a -> f b 
functorialFmap f m = fmap f m
-- functorialFmap  andOne [4, 5, 6]    

functorialBind ::  (Foldable t, Functor t) => (a -> [b]) -> t a -> [b]
functorialBind f m = concat $ fmap f m
-- functorialBind  andOne [4, 5, 6]    

monadicFmap :: Monad m => (a -> m b) -> m a -> m (m b)
monadicFmap f m = fmap f m
--  monadicFmap  andOne [4, 5, 6]    

-- aka (>>=)
monadicBind :: Monad m => (a -> m b) -> m a -> m b 
monadicBind f m = join (fmap f m)
--  monadicBind  andOne [4, 5, 6]

 --(Just (Sum 2)) >> (Just (Sum 1))

 --join (putStrLn <$> getLine)

-- using do notation with lists, 
-- is equivalent to using a list comprehension, plus an additional "flattening" operation, performed internally.
twiceWhenEvenDoNotation :: [Integer] -> [Integer] 
twiceWhenEvenDoNotation xs = 
    do 
        x <- xs 
        if even x
            then [x*x, x*x] 
            else [x*x]

twiceWhenEvenListComprehension :: [Integer] -> [Integer]
twiceWhenEvenListComprehension  
    xs = concat [if even x 
                 then [x*x, x*x] 
                 else [x*x] 
                | x <- xs]            

twiceWhenEvenOrNone :: [Integer] -> [Integer] 
twiceWhenEvenOrNone xs = do
    x <- xs 
    if even x
        then [x*x, x*x] 
        else []             