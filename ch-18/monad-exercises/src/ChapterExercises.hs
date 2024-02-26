module
    ChapterExercises
        where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

import Control.Monad (join, ap)    
 
data Nope a = 
    DoingNothing       
    deriving (Eq, Show)

instance Functor Nope 
    where
        fmap _ DoingNothing  = DoingNothing 

instance Applicative Nope
    where
        pure _ = DoingNothing
        (<*>) _ DoingNothing = DoingNothing      

instance Monad Nope
    where
        return = pure                  
        (>>=) DoingNothing _ = DoingNothing

-- Arbitrary instance for Nope
instance Arbitrary (Nope a) 
    where
        arbitrary = pure DoingNothing

instance Eq a => EqProp (Nope a) 
    where (=-=) = eq

nopeTest :: IO ()
nopeTest = do
    let trigger = undefined :: Nope (Int, String, Int) 
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    
--    
-- =======================================================           
--

data MockEither b a = 
  MockLeft a
  | MockRight b        
  deriving (Eq, Show)

instance Functor (MockEither b)  
    where
        fmap f (MockLeft l) = MockLeft (f l)
        fmap f (MockRight r) = MockRight r
--  fmap (+3) $ MockLeft 12        
--  fmap (+3) $ MockRight "No"

instance Applicative (MockEither b)
    where
        pure = MockLeft 
        (<*>) (MockLeft f)  f' = fmap f f'
        (<*>) (MockRight f)  _ = (MockRight f)

instance Monad (MockEither b)
    where
        return = pure -- (return  1) ::MockEither String Int
        (>>=) (MockLeft a) f = f a -- (MockLeft 1) >>= (\n -> MockLeft (1+n))
        (>>=) (MockRight a ) _ = MockRight a --  (MockRight "Out") >>= (\n -> MockLeft (1+n))
        
-- specialized    
-- instance Arbitrary (MockEither String Int) where
--     arbitrary = do 
--         intVal <- elements [0..99::Int]
--         strVal <- vectorOf 5 (elements ['A'..'Z'])
--         oneof [return (MockLeft intVal), return (MockRight strVal)]

-- instance Arbitrary (MockEither String (Int -> Int)) where
--     arbitrary = do
--         fun <- arbitrary :: Gen (Int -> Int)
--         return (MockLeft fun)

-- polymorphic 1
{-
instance (Arbitrary a, Arbitrary b) => Arbitrary (MockEither b a) 
    where
     arbitrary = oneof
        [ MockLeft <$> arbitrary
        , MockRight <$> arbitrary
        ]
-}
-- polymorphic 2
instance (Arbitrary a, Arbitrary b) => Arbitrary (MockEither b a) 
    where
     arbitrary = frequency [ 
        (1, MockLeft <$> arbitrary), 
        (1, MockRight <$> arbitrary)]
        
instance (Eq a, Eq b) => EqProp (MockEither b a) 
    where
        (=-=) = eq

mockEitherTest :: IO ()
mockEitherTest = do
    let trigger = undefined :: MockEither String (Int, Int, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    
--    
-- =======================================================           
--

-- The Identity type is a simple wrapper around a value
newtype Identity a  = 
    Identity a 
    deriving (Eq, Ord, Show)

instance (Semigroup a) => Semigroup (Identity a)
     where 
        (<>) (Identity i) (Identity i') = Identity (i <> i')

instance Monoid a => Monoid (Identity a)
    where 
        mempty  = Identity mempty  
        mappend = (<>)

instance Functor Identity 
    where
        fmap f (Identity i) = Identity (f i)
     
instance Applicative Identity 
    where 
        pure = Identity
        (<*>) (Identity f) (Identity v) = Identity (f v)

instance Monad Identity 
    where 
        return = pure
        (>>=) (Identity v) f =  f v

instance (Arbitrary a ) => Arbitrary (Identity a) 
    where
     arbitrary =  Identity <$> arbitrary
        
instance (Eq a) => EqProp (Identity a) 
    where
        (=-=) = eq

identityTest :: IO ()
identityTest = do
    let trigger = undefined :: Identity (Int, Int, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    
    -- Identity (Sum 2) <> Identity (Sum 3)   
    -- (+3) <$> Identity (Sum 2) 
    -- mempty <$> Identity (Sum 2) 

--    
-- =======================================================           
--

flatMap :: Monad m => m (m a) -> m a   
--option 1: 
-- flatMap m = join m 
--option 2: 
-- flatMap m = m >>= id
flatMap m = join $ fmap id  m

-- flatMap ( Just( Just 3) )
-- flatMap [[1, 2], [], [3]]


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f  m = fmap f m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--option 1: 
-- l2 f m m' = pure f <*> m <*> m'
-- option 2:
-- l2 f m m' = m >>= (\a ->  pure (f a) <*> m') 
-- option 3:
-- l2 f m m' = m >>= (\a ->  
--         m' >>= (\b -> return $ (f a) b) 
--     )
-- option 4:
l2 f m m' = m >>= (\a -> fmap (f a)  m') 


a :: Monad m => m a -> m (a -> b) -> m b    
a m f = ap f m
-- a (Just 3) (Just (+1)) 

flipType :: (Monad m) => [m a] -> m [a]    
flipType  = foldr (\a m -> (fmap (:) a) <*> m ) (pure mempty) 
-- flipType [Just 5, Just 10, Just 15]

-- meh [1,2,3] (\n -> Just (5*n)) = Just [5,10,15]
meh :: Monad m => [a] -> (a -> m b) -> m [b] 
meh xs f = flipType $ fmap f xs


