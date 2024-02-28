module Lib (
  someFunc,
  Option (..), 
  Trivial, 
  Identity(..), 
  Two(..), 
  TwoTypes, 
  Three(..), 
  ThreeTypes, 
  Four(..), 
  FourTypes, 
  BoolConj(..), 
  BoolDisj(..),
  Or(..),
  OrType,
  Compose (..),
  Combine (..),
  Validation (..),
  ValidationType,
  AccumulateRight (..),
  AccumulateBoth (..)
  
  ) where

{-
  A Semigroup is a set (type in Haskell) that is closed under an associative binary operation. 
   (<>) :: a -> a -> a
  "closed under"
     arguments and output will always inhabit the same set (type in Haskell)
-}

import Test.QuickCheck
import Data.Monoid

  
someFunc :: IO ()
someFunc = putStrLn "Hi !!!"

data Option m = 
 None
 | Some m 
 deriving (Eq, Show)

instance Functor Option
  where -- <$>, fmap
    fmap f (Some a) =  Some (f a)
    fmap _ None =  None

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

instance  (Arbitrary a) => Arbitrary (Option a) 
  where 
    arbitrary = oneof [Some <$> arbitrary, pure None]
       
-- ===========================================================================

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial 
  where 
    _ <> _ = Trivial

instance Arbitrary Trivial 
  where 
    arbitrary = return Trivial

-- ===========================================================================

newtype Identity a = Identity a 
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a)
  where 
    (Identity a) <> (Identity b) = Identity (a <> b)
   
instance  (Arbitrary a) => Arbitrary (Identity a) 
  where 
    arbitrary =  Identity <$> arbitrary

-- ===========================================================================

data Two a b = Two a b
  deriving (Eq, Show)

-- we can remove "(Semigroup a, Semigroup b) => .." if we don't <> a and b 
instance  (Semigroup a, Semigroup b) => Semigroup (Two a b)
  where 
    -- as such, this cannot pass associativity test. We would have to constrain our types  a b somewhat
    -- (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')
    (<>) (Two a _) (Two _ b') = Two a b' -- 
   
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) 
 where
  arbitrary = do
    t1 <- arbitrary 
    t2 <- arbitrary
    return (Two t1 t2)

type TwoTypes = Two (Product Int) (Product Float)  -> Two (Product Int) (Product Float)  -> Two (Product Int) (Product Float) -> Bool  

-- ===========================================================================

data Three a b c = Three a b c
  deriving (Eq, Show)
-- we can remove "(Semigroup a, Semigroup b ...) => .." if we don't <> a, b and c
instance  (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c)
  where 
   -- as such, this cannot associativity test. We would have to constrain our types  a b c somewhat
   --  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')
    (<>) (Three a _ c) (Three _ b' _) = Three a b' c
   
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) 
 where
  arbitrary = do
    t1 <- arbitrary 
    t2 <- arbitrary
    t3 <- arbitrary
    return (Three t1 t2 t3)

type ThreeTypes = 
     Three (Product Int) (Sum Float) (String) 
  -> Three (Product Int) (Sum Float) (String) 
  -> Three (Product Int) (Sum Float) (String) 
  -> Bool  

-- ===========================================================================

data Four a b c d = Four a b c d
  deriving (Eq, Show)
-- we can remove "(Semigroup a, Semigroup b ...) => .." if we don't <> a, b, c and d
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d)
  where 
  -- as such, this cannot pass the associativity test. We would have to constrain our types  a b c d somewhat
  --  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')
    (<>) (Four a b _ _) (Four _ _ c' d') = Four a b c' d'
   
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) 
 where
  arbitrary = do
    t1 <- arbitrary 
    t2 <- arbitrary
    t3 <- arbitrary
    t4 <- arbitrary
    return (Four t1 t2 t3 t4)

type FourTypes = 
     Four (Product Int) (Sum Float) (Option (Sum Int)) (String) 
  -> Four (Product Int) (Sum Float) (Option (Sum Int)) (String) 
  -> Four (Product Int) (Sum Float) (Option (Sum Int)) (String) 
  -> Bool 

-- ===========================================================================

newtype BoolConj = 
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj
  where
    (<>) (BoolConj True) (BoolConj True) = BoolConj True
    (<>) _                _              = BoolConj False

instance Arbitrary BoolConj
  where
   arbitrary = oneof [return (BoolConj True) , return (BoolConj False)]

  -- ===========================================================================

newtype BoolDisj = 
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj
  where
    (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
    (<>) _                _                = BoolDisj True

instance Arbitrary BoolDisj
  where
   arbitrary = oneof [return (BoolDisj True) , return (BoolDisj False)]

-- ===========================================================================
-- Is preferential to Snd
data Or a b = 
  Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b)
  where 
    (<>) (Fst _) (Snd b) = Snd b
    (<>) (Fst a ) (Fst _) = Fst a 
    (<>) (Snd a) _ = Snd a
   
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) 
 where
  arbitrary = do
    t1 <- arbitrary 
    t2 <- arbitrary
    oneof [return(Fst t1), return (Snd t2) ]

type OrType = 
  Or (Product Int) (Product Float)  
  -> Or (Product Int) (Product Float)  
  -> Or (Product Int) (Product Float) 
  -> Bool  

-- ===========================================================================
 
-- usage:
-- let f = Combine (\n -> Sum (n + 1))
-- let g = Combine (\n -> Sum (n - 1))
-- let c = getCombined (f <> g) 
--  c (0::Int)

newtype Combine a b =
  Combine { getCombined :: (a -> b) }
  
instance (Semigroup b) => Semigroup (Combine a b ) 
  where                           -- combine output of f and output of g
    (<>) (Combine f) (Combine g) = Combine (\x -> ((f x) <> (g x)))

-- ===========================================================================

--  usage:
--  let f = Compose (+1)
--  let g = Compose (+3)
--  let c = getComposed (f <> g)
--  c (3::Int)
--  7

newtype Compose a =
  Compose { getComposed :: (a -> a) }
  
instance Semigroup (Compose a ) 
  where                           -- output of g is input for f
    (<>) (Compose f) (Compose g)  =  Compose (\x -> f (g x)) 
  
-- ===========================================================================

data Validation a b = 
  Failed a 
  | Succeeded b 
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) 
  where
    (<>) (Failed a) (Failed a')       =  Failed (a <> a')
    (<>) (Failed a) (Succeeded _)     =  Failed a 
    (<>) (Succeeded _) (Failed a')    =  Failed a' 
    (<>) (Succeeded b) (Succeeded b') =  Succeeded (b)

-- arbitrary instance for  Validation having polymorphic types a &  b 
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b )
--   where
--    arbitrary = oneof [Succeeded <$> arbitrary, Failed  <$> arbitrary]

-- arbitrary instance specifically for Validation String (Sum Int)
instance  Arbitrary (Validation String (Sum Int) )
  where
    arbitrary = do 
      s <- vectorOf 5 (elements['a'..'z'])
      n <- elements [0,1..9]
      oneof [return (Failed s), return (Succeeded (Sum n))]
    
type ValidationType  = Validation String (Sum Int) -> Validation String (Sum Int)  -> Validation String (Sum Int)  -> Bool

-- ===========================================================================

newtype AccumulateRight a b = 
  AccumulateRight (Validation a b) 
  deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AccumulateRight a b) 
  where
    (<>) (AccumulateRight f) (AccumulateRight s) =  AccumulateRight s
-- (AccumulateRight (Failed "404")) <> (AccumulateRight (Succeeded (Sum 200)))       
-- ===========================================================================

newtype AccumulateBoth f s = 
  AccumulateBoth (Validation f s) 
  deriving (Eq, Show)

instance (Semigroup f, Semigroup s) => Semigroup (AccumulateBoth f s) 
  where
    (<>) (AccumulateBoth f) (AccumulateBoth s) =   AccumulateBoth (f <> s)
-- (AccumulateBoth (Failed "404")) <> (AccumulateBoth (Succeeded (Sum 200)))             
