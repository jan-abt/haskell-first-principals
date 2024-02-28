module Lib (
  Option (..), 
  sayHi, 
  GoodPrinciples(..),
  BadPrinciples(..),
  Or(..),
  Identity (..),
  Pair (..),
  Pair' (..),
  Three (..),
  Three' (..),
  Four (..),
  Four' (..), 
  Trivial
) where

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
-- Functor laws
-- composition law
-- fmap ((+3) . fmap (+1)) $ (Pls 2)  == fmap ((+3) . (+1)) $ Pls 2
-- identity law
-- fmap id (Pls 2) ==  id (Pls 2)
-- ==============================================================================
-- This datatype only has one data constructor containing a value we could fmap over, and that is Pls.
-- The other onbe is a nullary constructor.
-- There is no value to work with, inside the structure; there is only structure.
data FixMePls a =
  FixMe
  | OrElse
  | Pls a
  deriving (Eq, Show)

-- Substitution for clarity  (mind the different meanings of "f" depending on context)
-- function type definition
-- "f" in a function's type definition  refers to "Functor"  
-- fmap :: Functor f => (a -> b) -> f a   -> f b
-- function definition
-- "f" in a function definion usually refers to "function"
-- fmap f (Pls a) = Pls (f a) 

instance Functor FixMePls 
  where
   fmap _ FixMe = FixMe
   fmap _ OrElse = OrElse
   fmap f (Pls a) = Pls (f a)

data BadPrinciples a = 
  UncertainHeisenberg Int a 
    deriving (Eq, Show)

  
-- EXAMPLLE OF WHAT NOT TO DO
instance Functor BadPrinciples 
  where
     {-                                ^            
      The "n" is supposed to be part of the functorial structure of our functor "𝑓" . 
      The kind of any type constructor uxed to create Functor instances always has to be * -> * 
                *               ->   *
       [UncertainHeisenberg Int]     [a] 
      Therefore,  we can’t alter the value of n.
      While it will compile, it will will break composability and identity.  
    -}
    fmap f (UncertainHeisenberg n a) = UncertainHeisenberg (n+1) (f a)
--     (a->b)      f             a  =        f                   b


instance (Arbitrary a) => Arbitrary (BadPrinciples a)
  where 
    arbitrary =  fmap (UncertainHeisenberg 0) arbitrary

-- fmap (++ " Heisi") (UncertainHeisenberg 0 "Uncle")  
-- =========  the above will fail the function composition law =============
--

data GoodPrinciples a = 
  HappyHeisenberg Int a 
  deriving (Eq, Show)

instance Functor GoodPrinciples 
  where
    fmap f (HappyHeisenberg n a) = HappyHeisenberg (n) (f a)
--    (a->b)        f         a  =        f              b
-- Any type arguments of the functor (f) that are not the final type argument 
-- are considered part of the functor's structure, 
-- and the functions being lifted by the functor should be "oblivious" to those parts of the structure.
-- They should be carried over, unchanged so that the function composition law will hold

-- fmap (++ " Heisi") (Heisenberg 0 "Uncle")  

instance (Arbitrary a) => Arbitrary (GoodPrinciples a)
  where 
    arbitrary =  fmap (HappyHeisenberg 0) arbitrary
    -- arbitrary =  (HappyHeisenberg 0) <$> arbitrary

--  
-- =========  the above will pass the function composition law =============
--


data Or a b = 
  First a
  | Second b
  deriving (Eq, Show)

  {-
  With Or, we’re dealing with the independent possibility of two different values and types.
  Still, the same basic constraint applies:
  -}
instance Functor (Or a) 
  where
    fmap _ (First a) = First a --We’ve applied out the first argument, so now it’s part of the 𝑓 .
    fmap f (Second b) = Second (f b)

 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b)
  where 
    arbitrary = do
     el <- oneof [First <$> arbitrary, Second <$> arbitrary]
     return (el)
 
--  
-- ====================================================================================
-- 

newtype Identity a = 
  Identity a
  deriving (Eq, Show)

instance Functor Identity
  where
    fmap f (Identity a) = Identity (f a)
  -- fmap (+1) (Identity 5)
  -- fmap (+1) $ Identity (5::Int)

instance (Arbitrary a) => Arbitrary (Identity a)  
  where
     arbitrary = Identity <$> arbitrary
--  
-- ====================================================================================
-- 

data Pair a = Pair a a
  deriving (Eq, Show)

-- The type constructor of this type class has the  kind * -> *
-- There we can fmap over over element a and element a'
instance Functor Pair 
  where
    fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a) => Arbitrary (Pair a)  
  where
     arbitrary = do 
       a <-  arbitrary
       a' <- arbitrary
       return (Pair a a' )

--  
-- ====================================================================================
-- 

data Pair' a b = 
  Pair' a b  
  deriving (Eq, Show)

-- Given the above type declaration, the kind of the data type is * -> * -> * .
-- In order to be able to create an intance of a Functor our type needs to be of the kind * -> * .
-- Solution: attach the innermost "extra" element (type variable) to the "structure" we are fmapping over.
instance Functor (Pair' a) 
  where 
    --  fmap a given function "f" only over the second, or outer element of the pair 
    -- (because in the instance declaration we  "fixed" the first type parameter `a` to the structure).
    fmap f (Pair' a b) = Pair' a (f b)
    --                         ^
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair' a b)  
  where
     arbitrary = do 
       a <-  arbitrary
       b <- arbitrary
       return (Pair' a b)

--  
-- ====================================================================================
-- 

data Three a b c = 
  Three a b c
  deriving (Eq, Show) 

instance Functor (Three a b)  
  where
    fmap f (Three a b c)  = (Three a b (f c) )

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where 
    arbitrary = do
     a <- arbitrary     
     b <- arbitrary     
     c <- arbitrary      
     return (Three a b c)

--  
-- ====================================================================================
--

data Three' a b = 
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a)
  where
    fmap f (Three' a b b') = (Three' a (f b) (f b'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)
  where
    arbitrary = do
      a <- arbitrary    
      b <- arbitrary    
      b' <- arbitrary    
      return (Three' a b b')

--  
-- ====================================================================================
--

data Four a b c d = 
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c)
  where
    fmap f (Four a b c d) = (Four a b c (f d))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
    arbitrary = do
      a <- arbitrary    
      b <- arbitrary    
      c <- arbitrary    
      d <- arbitrary    
      return (Four a b c d)

--  
-- ====================================================================================
-- 


data Four' a b = 
  Four' a a a b      
  deriving (Eq, Show)

instance Functor (Four' a)
  where
    fmap f (Four' a a' a'' b) = (Four' a a' a'' (f b))  

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b)
  where
    arbitrary = do
      a   <- arbitrary      
      a'  <- arbitrary      
      a'' <- arbitrary      
      b   <- arbitrary      
      return (Four' a a' a'' b)

--  
-- ====================================================================================
-- 
{-
 We cannot form instances of Functor with this data type since the type constructor Trivial is of kind * .
 Functor is a type class that takes a type constructor of kind * -> * (a type constructor that takes one type argument)
  ghci> :i Functor 
  type Functor :: (* -> *) -> Constraint

-}
data Trivial = 
  Trivial
  deriving (Eq, Show)  

