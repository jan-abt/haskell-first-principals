module Lib (
  Option (..), 
  Boolsy(..), 
  Primary(..), 
  someFunc) where

import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "Hello!"

-- =============== OPTION MONOID (option's content is monoidal, too) ===============

data Option m = 
 None
 | Some m
 deriving (Eq, Show)

-- class constraint on 'm', indicating that 'm' also an 'appendable' Semigroup
instance Semigroup m => Semigroup (Option m)
 where 
  (<>) None m =  m    
  (<>) m None =  m    
  (<>) (Some m) (Some m')  = Some ( m <> m')  
 
-- class constraint on 'm', indicating that 'm' also an 'appendable' Semigroup 
instance Semigroup m => Monoid (Option m) 
 where 
  mempty = None

-- =============== BOOLSY MONOID  ===============

-- monomorphic data types 
data Boolsy =
 Truzy
 | Falsy
 deriving (Eq, Show)

instance Arbitrary Boolsy 
 where 
  arbitrary = frequency [ (1, return Falsy) , (1, return Truzy) ]

instance  Semigroup (Boolsy)
 where
  (<>) Falsy x = x 
  (<>) x Falsy = x 
  (<>) _ _  = Truzy  

instance Monoid Boolsy 
 where 
  mempty = Falsy
  
-- =============== OPTIONAL MONOID (with parameterized types whose content is NOT monoidal) ==============

newtype Primary a =
 Primary { getPrimary :: Option a } 
 deriving (Eq, Show)

-- no class constraint on 'm'
instance Semigroup (Primary m)
 where 
  (<>) (Primary None) m =  m    
  (<>) m (Primary None) =  m    
  (<>) m _ = m  

-- no class constraint on 'm'
instance Monoid (Primary m) 
 where 
  mempty = Primary None
 
-- 'Arbitrary' class constraint on 'a', ensuring that we can generate arbitrary values of type 'a'
instance Arbitrary a => Arbitrary (Option a)
  where 
    -- returns a "Gen having a" where "a" is an arbitrarily generated value wrapped into a randomly chosen "Some" or "None"
    arbitrary = oneof [Some <$> arbitrary, pure None]

-- 'Arbitrary' class constraint on 'a', ensuring that we can generate arbitrary values of type 'a'
instance Arbitrary a => Arbitrary (Primary a)
  where 
    -- returns a "Gen having a" where "a" is an arbitrarily generated value wrapped into the "Primary" constructor
    arbitrary = Primary <$> arbitrary


