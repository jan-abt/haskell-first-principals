module
    Dyad -- a "dyad" is a tuple with two elements.
        where

import Test.QuickCheck -- for "Arbitrary"

    

data Dyad a b = 
  Dyad a b 
  deriving (Eq, Ord, Show)
    
instance Functor (Dyad a) 
    where
        fmap f (Dyad a b) = Dyad a (f b)        

instance (Semigroup a, Semigroup b) => Semigroup (Dyad a b)
    where   
        (<>)(Dyad a b) (Dyad a' b') = Dyad (a<>a') (b<>b')

instance (Monoid a, Monoid b) => Monoid (Dyad a b)
    where
        mempty = Dyad mempty mempty
        mappend = (<>)

instance Monoid a => Applicative (Dyad a) 
    where                   -- Dyad (Sum {getSum = 0}) (Sum {getSum = 5})
        pure  = Dyad mempty -- pure 5 :: Dyad (Sum Int) (Sum Int) 
        (<*>) (Dyad a f) (Dyad a' b)  = Dyad (a<>a') (f b)
                            -- let f = pure (*5) :: Dyad (Sum Int) (Sum Int -> Sum Int) 
                            -- f <*> Dyad (Sum 1) (Sum 5)

instance Foldable (Dyad a) 
    where
        -- foldMap (\n -> Sum (5*n)) [1,2]
        -- foldMap (\n -> Sum (5*n)) (1,2)
        -- foldMap (\n -> Sum (5*n)) $ Dyad 1 2
        -- maps each element of a structure into a monoid, and combines the results
        foldMap f (Dyad _ b) = f b
        --  foldr (\n (Just a) -> Just (n+a)) (Just 2) $ Dyad 1 2
        foldr f acc (Dyad _ b) = f b acc

instance Traversable (Dyad a) 
    where 
        -- traverse (\n -> id n) [Just 1]
        traverse f (Dyad a b)= fmap (Dyad a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Dyad a b) 
    where
    arbitrary = Dyad <$> arbitrary <*> arbitrary
