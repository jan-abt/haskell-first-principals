module Lib (Option (..), sayHi) where


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
   mappend = (<>)

-- 
-- SAMPLE REPL USAGE
-- ghci> import Data.Monoid
-- ghci> Some ( Sum  3) <> Some ( Sum 3)
-- Some (Sum {getSum = 6})
--

-- ==============================================================================

-- Prelude> const <$> [1, 2, 3] <*> [9, 9, 9]
--  [const 1,const 2, const 3] <*> [9,9,9]
--  [1,1,1,2,2,2,3,3,3]

-- Prelude> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
--  Identity (const [1,2,3]) <*> Identity [9, 9, 9]
--  Identity [1,2,3]  


newtype Identity a = -- type constructor, has a kind "a"
  Identity a -- data/value constructor needs one argument of type "a" to become a fully realized "value"
  deriving (Eq, Ord, Show)

instance Functor (Identity) --functor needs a higher kinded type * -> *. We can achieve that by partially applying Identity
  where 
    fmap f (Identity va) = Identity(f va) -- va is a value of type a

instance Applicative (Identity)
  where 
    -- pure 1 :: Identity Int 
    pure = Identity -- s/a  pure va = Identity va (type variables va are implied)
    (<*>) (Identity f) (Identity va) = Identity(f va)

-- ==============================================================================

-- ghci> Constant (Sum 1) <*> Constant (Sum 2)
-- Constant {getConstant = Sum {getSum = 3}
-- ghci> Constant undefined <*> Constant (Sum 2)
-- Constant (Sum {getSum = *** Exception: Prelude.undefined
-- ghci> pure 1
-- 1
-- ghci> pure 1 :: Constant String Int
-- Constant {getConstant = ""}

data Constant a b = -- b is a phantom type variable.
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) 
  where 
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) 
  where 
    pure _ = Constant mempty
    (<*>) (Constant va) (Constant va') = Constant (va <> va')

-- ==============================================================================
