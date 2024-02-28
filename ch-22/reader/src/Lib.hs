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

--
-- ==============================================================================
--

