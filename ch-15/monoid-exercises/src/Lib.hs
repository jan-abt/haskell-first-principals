
module Lib (
  Option (..), 
  sayHi,
  TrivialAssoc,
  TrivialIdentity,
  Identity(..),
  Two(..),
  ConjunctiveBool(..),
  DisjunctiveBool(..),
  Mem(..), 
  CombineF(..),   
  ComposeF(..), 
  
) where
 
{-
  A monoid is a set (type in Haskell) that is "closed under" an associative binary operation and has an identity element. 
   mappend :: a -> a -> a
  "closed under"
     arguments and output will always inhabit the same set (type in Haskell)
-}

import Test.QuickCheck  


sayHi :: IO ()
sayHi = putStrLn "Hi from Lib!"

data Option m = 
 None
 | Some m
 deriving (Eq, Show)

-- fmap, <$>
instance Functor Option
  where 
    fmap f (Some a) =  Some (f a)
    fmap _ None =  None

-- To implement Semigroup and Monoid for the Option type, we need to decide on:
--  *  the behavior we want to see when combining the values of two Options (for Semigroups)
--  *  the identity element (for Monoids)

-- class constraint on 'm', indicating that 'm' is also an 'appendable' Semigroup
instance Semigroup m => Semigroup (Option m)
 where 
  (<>) None m =  m    
  (<>) m None =  m    
  (<>) (Some m) (Some m')  = Some ( m <> m')  
 
-- class constraint on 'm', indicating that 'm' is also an 'appendable' Semigroup 
instance Semigroup m => Monoid (Option m) 
 where 
  mempty = None

-- ==============================================================================

-- Since Trivial doesn't carry any meaningful information, the implementation will be very minimal.
  
data Trivial = Trivial deriving (Eq, Show)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialIdentity = Trivial -> Bool

instance Semigroup Trivial 
  where 
    _ <> _ = Trivial

instance Monoid Trivial 
  where  -- as we can see in the type declaration, there isn't a value constructor to represent empty values 
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial
  where  -- in this scenario, we are just generating the same thing over and over
   arbitrary = return Trivial

-- ==============================================================================

newtype Identity a = 
  Identity a 
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a)
  where
    (Identity a) <> (Identity a') = Identity (a <> a')

-- class constraint indicating that
-- the type a, which inhabits Identity 
-- must also be an instance of the Monoid type class, as well as the type Identity itself
-- This is why we can implement mempty with Identity mempty
instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

{-- Alternatively one could have implemented instances for each type of Identity
instance (Num a) => Monoid (Identity (Sum a))
  where
    mempty = Identity 0
    mappend = (<>)      

instance (Num a) => Monoid (Identity (Product a))
  where
    mempty = Identity 1
    mappend = (<>)      

instance Monoid (Identity String)
  where
    mempty = Identity ""
    mappend = (<>)      

 --}

-- produce arbitrary values of  type a
instance (Arbitrary a) => Arbitrary (Identity a)
  where 
    arbitrary =  Identity <$> arbitrary

-- ==============================================================================
-- (&&)
newtype ConjunctiveBool = 
  ConjunctiveBool Bool
    deriving (Eq, Show)

instance Semigroup (ConjunctiveBool)
  where
    ConjunctiveBool True <> ConjunctiveBool True = ConjunctiveBool True
    ConjunctiveBool False <> y = y -- satisy the associativity rule
    x <> ConjunctiveBool False = x -- satisy the associativity rule

instance Monoid (ConjunctiveBool) 
  where
    mempty = ConjunctiveBool False
    mappend = (<>)    

instance Arbitrary (ConjunctiveBool)
  where 
    arbitrary = do
      b <- oneof [return True, return False]
      return (ConjunctiveBool b )

-- ==============================================================================
-- (||)
newtype DisjunctiveBool = 
  DisjunctiveBool Bool
    deriving (Eq, Show)

instance Semigroup (DisjunctiveBool)
  where
    DisjunctiveBool True <> DisjunctiveBool _ = DisjunctiveBool True
    DisjunctiveBool False <> y = y -- satisy the associativity rule

instance Monoid (DisjunctiveBool) 
  where
    mempty = DisjunctiveBool False
    mappend = (<>)    

instance Arbitrary (DisjunctiveBool)
  where 
    arbitrary = do
      b <- oneof [return True, return False]
      return (DisjunctiveBool b )  

 -- ==============================================================================

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b)
   where 
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')
 
-- class constraint indicating that
-- the types a and b, which inhabit Two 
-- must also be instances of the Monoid type class, as well as the type Two itself.
-- This is why we can express Two mempty mempty
instance (Monoid a, Monoid b) => Monoid (Two a b)
  where 
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where
    arbitrary = do
      valA <- arbitrary
      valB <- arbitrary
      return (Two valA valB)

-- ==============================================================================

-- usage:  
-- let f = CombineF (\n -> Sum (n + 5))
-- let g = CombineF (\n -> Sum (n + 10))
-- let h = CombineF (\n -> Sum (n + 15))
-- applyToAll ( f <>  g <> h) $ (1::Sum Int)
-- applyToAll ( f <>  g) $ (mempty::Sum Int)
-- applyToAll (mappend f mempty) $ (1::Sum Int)
-- applyToAll ( f <>  mempty) $ (1::Sum Int)
newtype CombineF a b =
  CombineF { applyToAll :: (a -> b) }

instance (Semigroup f, Semigroup g) => Semigroup (CombineF f g)  
  where
    CombineF f <> CombineF g = CombineF (\x -> f x <> g x)

instance (Monoid f, Monoid g) => Monoid (CombineF f g)  
  where
    -- The mempty for functions (a -> b) is
    -- the function that always returns 
    -- the mempty value of the result type b
    mempty = CombineF (\_ -> mempty )
    -- The right/left identity laws state that 
    -- combining any value with mempty using (<>) should result in the original value
    --
    -- (\_ -> mempty) works here, reason:
    -- mempty <> (CombineF g) = (CombineF (\_ -> mempty)) <> (CombineF g)
    --                        =  CombineF (\n -> (\_ -> mempty) n <> f n)
    --                        =  CombineF (\n -> mempty <> f n)
    --                        =  CombineF (\n -> f n)
    --                        =  CombineF f
    mappend = (<>)

-- ==== INSTANCES & TYPES FOR QUICKCHECK TESTS ====  
instance (CoArbitrary a, Arbitrary a, CoArbitrary b, Arbitrary b) => Arbitrary (CombineF a b) where
    arbitrary = do
        func <- arbitrary
        return (CombineF func)  

instance Show (CombineF a b) where
    show _  =  "ComposeF f"

  -- ==============================================================================  

-- usage:  
-- let f = ComposeF (\n -> n + 5)
-- let g = ComposeF (\n -> n + 10)
-- let h = ComposeF (\n -> n + 15)
-- composeAll (f <> g <> h) $ (1::Sum Int)

newtype ComposeF a = 
  ComposeF { composeAll :: (a -> a) }
-- let f = ComposeF (\n -> n + 5)
--  (composeAll (mempty <> f) $ 2::(Sum Int)) 
instance Semigroup (ComposeF a)
  where
    ComposeF f <>  ComposeF g = ComposeF (\x -> f (g x))

instance (Monoid a) => Monoid (ComposeF a)
   where
    -- The right/left identity laws state that 
    -- combining any value with mempty using (<>) should result in the original value
    
    -- 
    -- mempty = ComposeF id works, proof: 
    -- mempty <> (ComposeF f) = (ComposeF id) <> (ComposeF f)
    --                        =  ComposeF (\x -> id (f x)) <-- id, here ensures that the other function's is returned
    --                        =  ComposeF (\x -> f x)
    --                        =  ComposeF f
    --
    --
    -- mempty = ComposeF (\_ -> mempty) is not going to work, reason:
    -- mempty <> (ComposeF f) =  (ComposeF (\_ -> mempty)) <> (ComposeF f)
    --                        =   ComposeF (\x -> (\_ -> mempty) (f x) ) <-- the input, passed in from the other function, is always ignored - mempty is the only value ever returned
    --                        =   ComposeF (\x -> mempty)
    --                        =   ComposeF mempty
    --
    mempty = ComposeF id 
    mappend = (<>)

-- ==== INSTANCES & TYPES FOR QUICKCHECK TESTS ====  
instance (CoArbitrary a, Arbitrary a) => Arbitrary (ComposeF a) where
    arbitrary = do
        func <- arbitrary
        return (ComposeF func)  

instance Show (ComposeF a) where
    show _ =  "ComposeF f"


-- ==============================================================================  
-- sanity checks 
-- let f = Mem (\n -> ("Hello", n))
-- let g = Mem (\n -> (" Jan", n))
-- let c = f <> g
-- (runMem c) $ (1::Sum Int)

-- let mem = Mem $ \s -> ("hi", s + 1)
-- runMem (mem <> mempty) $ (0::Sum Int)
-- runMem (mempty <> mem) $ (0::Sum Int)
-- (runMem mempty 0 ):: (String, Sum Int)
-- (runMem mempty mempty ):: (String, Sum Int)
--  mempty :: (String, Sum Int)
-- (runMem (mem <> mempty) $ (0::Sum Int)) == (runMem mem $ (0::Sum Int))
-- (runMem (mempty <> mem) $ (0::Sum Int)) == (runMem mem $ (0::Sum Int))

newtype Mem n s = 
  Mem { runMem :: n -> (s, n) }

instance (Semigroup n, Semigroup s) => Semigroup (Mem n s)  
  where -- an instance of Semigroup for (a,b) existsts on Hackage, so tuple (a,b) knows how to combine
        -- https://hackage.haskell.org/package/base-4.19.0.0/docs/src/GHC.Base.html#line-428
    (Mem f) <> (Mem g) = Mem (\n -> f(n) <> g(n) ) 

instance (Monoid n, Monoid s) => Monoid (Mem n s)
  where
    -- The right/left identity laws state that 
    -- combining any value with mempty using (<>) should result in the original value
    --
    -- (\_ -> mempty) works here, reason:
    -- mempty <> (Mem f) = (Mem (\_ -> mempty)) <> (Mem f)
    --                   =  Mem (\n -> (\_ -> mempty) n <> f n)
    --                   =  Mem (\n -> mempty <> f n)
    --                   =  Mem (\n -> f n)
    --                   =  Mem f
    mempty =  Mem (\_ -> mempty) 
    mappend = (<>)


-- ==== INSTANCES & TYPES FOR QUICKCHECK TESTS ====  
instance (CoArbitrary a, Arbitrary a, CoArbitrary b, Arbitrary b) => Arbitrary (Mem a b) where
    arbitrary = do
        func <- arbitrary
        return (Mem func)  

instance Show (Mem a b) where
    show _  =  "Mem f"


-- =============================================================================
