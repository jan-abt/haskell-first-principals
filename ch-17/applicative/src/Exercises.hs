module
    Exercises
        where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Control.Applicative (liftA3)

data Tuple a b = 
    Tuple ((,) a b )
    deriving (Eq, Show)

instance Functor (Tuple a)
    where   
        fmap f  (Tuple (a, b)) = Tuple (a, (f b))
        -- example |  fmap (+1) $ Tuple ("Jan", 1) 

instance (Semigroup a, Semigroup b ) => Semigroup (Tuple a b)    
    where
        ( <> ) (Tuple (a, b)) (Tuple (a', b')) = Tuple (a<>a', b<>b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b ) => Monoid ( Tuple a b ) 
    where
        mempty = Tuple (mempty, mempty)
        mappend = (<>)

instance (Monoid a) => Applicative (Tuple a)
    where
        pure v = Tuple( mempty, v) 
        -- for the first element use any type that satisfies the (monoidal) type constraint
        -- the first element needs to understand how to be "mempty".
        -- examples | pure 1 :: Monoid a => Tuple a Int
        --          | pure 1 :: Tuple (Sum Int) Int
        --          | pure 1 :: Tuple String Int
        --          | pure 1 :: (Num n) => Tuple String n
        (<*>) (Tuple fs ) (Tuple vs) = let a = fst vs
                                           f = snd fs
                                           b = snd vs   
                                       in Tuple (a, f b)
        -- | let p = pure (+1) :: Monoid a => Tuple a (Int -> Int)
        -- |     p <*> Tuple ("Jan", 1)
        -- | OR
        -- | pure (+1) <*> Tuple ("Jan", 1)     

-- ================================================================================

-- | :t Func
-- | Func :: (a -> b) -> Func a b
--
newtype Func a b = 
    Func (a -> b)

instance Functor (Func a)
    where  
        fmap f ( Func fs ) = Func (f . fs)
-- | example        
-- | f1 = (+1)     
-- | f2 = (*2)
-- | Func fn = fmap f1 $ Func f2        
-- | fn 5

instance  Applicative (Func a)
    where
        pure f = Func (\_ -> f) 
        (<*>) (Func f) (Func v) = Func (\a -> (f a)  (v a))
-- | example        
--   x = pure f1 :: ( Num a) => Func Int  (a -> a)
-- | plus1 = (+1)     
-- | times2 = (*2)
-- | Func fn = pure plus1 <*> Func times2        
-- | fn 5

-- ================================================================================

data PartiallyAppliedTuple arg a b = 
    PAT (arg -> (a, b))

instance Functor (PartiallyAppliedTuple x a)  
    where   --  PAT ("Jan ", )
        fmap f (PAT pt) =      
            PAT (\x -> let tp = pt x
                           a  = fst tp
                           b  = f (snd tp)
                        in (a,  b)) 
             
-- | let fmappedPAT = fmap (* 2) $ PAT ("Jan ", )
-- | PAT pf = fmappedPAT
-- | pf 3

instance (Monoid a) => Applicative (PartiallyAppliedTuple x a)
    where 
        pure v = PAT (\_ -> (mempty, v)) 
        (<*>) (PAT f) (PAT pt) = 
            PAT (\x -> let (_, fn) = f x
                           (a, a1) = pt x                             
                       in  (a, (fn a1)))

-- | let applicativePAT = pure (* 2) <*> PAT ("Jan ", )
-- | PAT pf = applicativePAT
-- | pf 3   

-- ================================================================================

newtype Identity a = 
    Identity a 
    deriving (Eq,Show)

instance Functor (Identity)
    where
        fmap f (Identity a) = Identity (f a)

instance Applicative (Identity)
    where
        pure a = Identity a
        (<*>) (Identity f) (Identity a) = Identity (f a)

-- FOR QUICK CHECK DATA GENERATION    
instance Arbitrary a => Arbitrary (Identity a) 
    where  
        arbitrary =  Identity <$> arbitrary 

instance Eq a => EqProp (Identity a) 
    where 
        (=-=) = eq 

-- sample usage:    
-- quickBatch $ applicative (Identity ("a", "b", "c") )

-- ================================================================================

data Pair a = 
    Pair a a 
        deriving (Eq, Show)

instance Functor Pair       
    where
        fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative (Pair)
    where   
        pure x = Pair x x
        (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance Arbitrary a => Arbitrary (Pair a) 
    where  
        arbitrary = pure <$> arbitrary 

instance Eq a => EqProp (Pair a) 
    where 
        (=-=) = eq     

-- sample usage:    
-- quickBatch $ applicative ( undefined :: Pair (Int, Int, Int))

-- ================================================================================


data Two a b = 
    Two a b 
        deriving (Eq, Show)

instance Functor (Two a)       
    where
        fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a)
    where   
        pure b = Two mempty b
        (<*>) (Two f f' ) (Two a b) = Two a (f' b)

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Two a b) 
    where  
        arbitrary = pure <$> arbitrary 

instance (Eq a, Eq b)=> EqProp (Two a b) 
    where 
        (=-=) = eq     

-- sample usage:    
-- quickBatch $ applicative ( undefined :: Two  String (Int, Int, Int)  )

-- ================================================================================

data Three a b c = 
    Three a b c 
        deriving (Eq, Show)

instance Functor (Three a b)       
    where
        fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative  (Three a b)       
    where   
        pure c = Three mempty mempty c
        (<*>) (Three f f' f'') (Three a b c) = Three a b (f'' c)

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance (Monoid a, Monoid b, Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)       
    where  
        arbitrary = pure <$> arbitrary 

instance (Eq a, Eq b, Eq c)=> EqProp (Three a b c) 
    where 
        (=-=) = eq     

-- sample usage:    
-- quickBatch $ applicative ( undefined :: Three  String  String (Int, Int, Int)  )

-- ================================================================================

-- ================================================================================

data Three' a b = 
    Three' a b b 
        deriving (Eq, Show)

instance Functor (Three' a)       
    where
        fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative  (Three' a)       
    where   
        pure b = Three' mempty b b
        (<*>) (Three' f g g') (Three' a b b') = Three' a (g b) (g' b')

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance (Monoid a, Monoid b, Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)       
    where  
        arbitrary = pure <$> arbitrary 

instance (Eq a, Eq b)=> EqProp (Three' a b) 
    where 
        (=-=) = eq     

-- sample usage:    
-- quickBatch $ applicative (undefined :: Three' String (Sum Int, Sum Int, Sum Int)) 


-- ================================================================================

data Four a b c d = 
    Four a b c d
        deriving (Eq, Show)

instance Functor (Four a b c)       
    where
        fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative  (Four a b c)       
    where   
        pure d = Four mempty mempty  mempty d
        (<*>) (Four _ _ _ f) (Four a b c d) = Four a b c (f d)

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance (Monoid a, Monoid b, Monoid c, Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)       
    where  
        arbitrary = pure <$> arbitrary 

instance (Eq a, Eq b, Eq c, Eq d)=> EqProp (Four a b c d) 
    where 
        (=-=) = eq     

-- sample usage:    
--  quickBatch $  applicative (undefined :: Four String [Sum Int] (Product Int) (Sum Int, Sum Int, Sum Int)) 

-- ================================================================================

data Four' a b = 
    Four' a a a b
        deriving (Eq, Show)

instance Functor (Four' a)       
    where
        fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative  (Four' a)       
    where   
        pure d = Four' mempty mempty  mempty d
        (<*>) (Four' _ _ _ f) (Four' a a' a'' b) = Four' a a' a'' (f b)

--  pure (+3) <*> Pair 2 3
--  Pair (+2) (*8) <*> Pair 2 3

-- FOR QUICK CHECK DATA GENERATION    
instance (Monoid a, Monoid b, Arbitrary a, Arbitrary b) => Arbitrary (Four' a b)       
    where  
        arbitrary = pure <$> arbitrary 

instance (Eq a, Eq b)=> EqProp (Four' a b) 
    where 
        (=-=) = eq     

-- sample usage:    
-- quickBatch $ applicative (undefined :: Four' String (Sum Int, Sum Int, Sum Int))

-- ================================================================================
{-
1. Given the following sets of consonants and vowels:
     stops = "pbtdkg"
    vowels = "aeiou"
a) Using a for-comprehension, 
   write a function called "combos",
  that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations.    

b) reimplement the "combos" function using liftA3 from Control.Applicative.

-}

combos :: [a] -> [b] -> [c] -> [(a, b, c)] 
combos ss vs ss' = [(s, v, s') | s<-ss, v<-vs, s'<-ss' ]      

listSVSCombo = combos stops vowels stops
    where
        stops = "pbtdkg"
        vowels = "aeiou"


liftSVSCombo :: [(Char, Char, Char)]    
liftSVSCombo = liftA3 combo stops vowels stops
    where
        stops = "pbtdkg"
        vowels = "aeiou"

combo :: a -> b -> c -> (a, b, c) 
combo s v s' = (s, v, s')
    