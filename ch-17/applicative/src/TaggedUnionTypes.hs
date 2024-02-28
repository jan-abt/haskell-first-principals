module
    TaggedUnionTypes
        where

{-
    A "tagged union", also known as a "variant type" or "discriminated union", 
    is a type that can hold values of different types, 
    but each value has a tag (Left or Right) indicating which side of the union the value belongs to.
    Either : 
        Left "My error message"
        Right 1000
-}

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Goal m a = 
    Missed m
    | Achieved a
    deriving (Eq, Show)

-- to be a proper Functor (kind) we bind the first of our two types, "m" to the structure
-- of the data constructor that is being applied to it, in this case Missed 
-- therefore,  a function cannot be applied to the "m of (Missed m), later in fmap
instance Functor (Goal a) 
    where 
        fmap _ (Missed m) = Missed m
        fmap f (Achieved a) = Achieved (f a) -- "a" is available to be fmapped over

instance Applicative (Goal a) 
    where 
        pure g = Achieved g --pure (1):: (Goal a Int) | pure (+1):: (Goal a (Int->Int))
        (<*>) (Achieved f) (Achieved a) = Achieved (f a)  -- pure ( (^) 2) <*> Achieved 3
        (<*>) (Missed m) _              = Missed m --  Missed ("dag yo") <*> Achieved 3
        (<*>) _ (Missed m)              = Missed m


-- ======================  INSTANCES FOR QUICK CHECK ===========================                
instance (Arbitrary m, Arbitrary a, CoArbitrary a) => Arbitrary (Goal m a) where
    arbitrary = do
        a1 <- oneof [Achieved <$> arbitrary, Missed <$> arbitrary]
        a2 <- oneof [Achieved <$> arbitrary, Missed <$> arbitrary]
        a0 <- arbitrary :: Gen (a -> a -> a)
        -- generates something like: 
        -- pure (+) <*> Achieved 10 <*> Missed "Sorry"
        return (pure a0 <*> a1 <*> a2)
    
instance (Eq m, Eq a) => EqProp (Goal m a) where (=-=) = eq
   
--  quickBatch $ applicative ( Missed 'N' :: Goal Char (Int, Int, Int)) 
--  quickBatch $ applicative (Achieved (1, 2, 3) :: Goal Char (Int, Int, Int))
--  quickBatch $ applicative (undefined :: Goal Char (Int, Int, Int))
--  using "undefined" as a placeholder for the type constructor Goal. 
--  undefined :: Goal String (Int, Int, Int) 


-- ==========================================================================================
-- ==========================================================================================
-- ==========================================================================================


data Validation i p = 
    Invalid i
    | Passed p 
    deriving (Eq, Show)

-- to be a proper Functor (kind) we bind the first of our two types, "i" to the structure
-- of the data constructor that is being applied to it, in this case Invalid 
-- therefore,  a function cannot be applied to the "i" of (Invalid i), later in fmap
instance Functor (Validation a) 
    where 
        fmap _ (Invalid i) = Invalid i
        fmap f (Passed p) = Passed (f p) -- "p" is available to be fmapped over

instance (Semigroup i) => Semigroup (Validation i p)
    where
        (<>)(Invalid i) (Invalid i') = Invalid (i <> i')

-- This is different, here we are combining two invalid results
instance Monoid i => Applicative (Validation i) 
    where
        pure v = Passed v  -- pure 1 :: Validation [String] Int
        (<*>) (Passed f)  (Passed a)   = Passed (f a)   -- pure (+1) <*> Passed 1
        (<*>) (Invalid m) (Invalid m') = Invalid (m <> m')
        (<*>) (Invalid m) _            = Invalid m 
        (<*>) _           (Invalid m)  = Invalid m --pure (+1) <*> Invalid ["Jan"]


 -- ======================  INSTANCES FOR QUICK CHECK ===========================   
instance (Monoid i, Arbitrary i, Arbitrary p, CoArbitrary p) => Arbitrary (Validation i p) where
    arbitrary = do 
        a1 <- oneof [frequency[(1,Passed <$> arbitrary), (10, Invalid <$> arbitrary)]]
        a2 <- oneof [frequency[(1,Passed <$> arbitrary), (10, Invalid <$> arbitrary)]]
        a0 <- arbitrary :: Gen (p -> p -> p)
        -- generates something like: 
        -- pure (+) <*> Achieved 10 <*> Missed "Sorry"
        return (pure a0 <*> a1 <*> a2)

-- Assuming Eq instances for 'Invalid' and 'Passed'
instance (Eq i, Eq p) => EqProp (Validation i p) where
  (=-=) = eq 

-- ghci> pure (+) <*> Passed 4 <*> Passed 5
-- Passed 9
-- ghci> pure (+) <*> Passed 4 <*> Invalid ["Jan"]
-- Invalid ["Jan"]
-- ghci> pure (+) <*> Invalid ["Mr."] <*> Invalid ["Abt"]
-- Invalid ["Mr.","Abt"]

     
--  quickBatch $ applicative ( Invalid ["Sorry"] :: Validation [String] (Int, Int, Int)) 
--  quickBatch $ applicative (Passed (1, 2, 3) :: Validation [String] (Int, Int, Int))
--  quickBatch $ applicative ( undefined :: Validation [String] (Int, Int, Int))
--  using "undefined" as a placeholder for the type constructor Goal. 
--  undefined :: Validation [String] (Int, Int, Int) 


-- ==========================================================================================
-- ==========================================================================================
-- ==========================================================================================
   