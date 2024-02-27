module 
	Lib (Option (..), sayHi) 
		where

sayHi :: IO ()
sayHi = putStrLn "Hi from Lib!"


data Option m =
 None
 | Some m
 deriving (Eq, Show)

-- class constraint on 'm' indicates that it also must be a Semigroup, e.g. it understands (<>)
instance Semigroup m => Semigroup (Option m)
 where
  (<>) :: Semigroup m => Option m -> Option m -> Option m
  (<>) None m =  m
  (<>) m None =  m
  (<>) (Some m) (Some m')  = Some ( m <> m')

-- class constraint on 'm' indicates that it also must be a Semigroup, e.g. it understands (<>)
instance Semigroup m => Monoid (Option m)
 where
  mempty :: Semigroup m => Option m
  mempty = None
  
  mappend :: Semigroup m => Option m -> Option m -> Option m
  mappend = (<>)

-- fmap, <$>
instance Functor Option
  where
    fmap :: (a -> b) -> Option a -> Option b
    fmap f (Some a) =  Some (f a)
    fmap _ None =  None


instance Applicative Option
    where
        pure :: a -> Option a
        pure  =  Some

        (<*>) :: Option (a -> b) -> Option a -> Option b
        Some f <*> g = fmap f g
        None  <*> _  = None

instance Foldable Option
    where
        foldMap :: Monoid m => (a -> m) -> Option a -> m
        foldMap _ None     = mempty
        foldMap f (Some a) = f a

instance Traversable Option
    where
        traverse :: (Applicative f  )=> (a -> f b) -> Option a -> f (Option b)
        traverse f (Some a) =  Some  <$> f a
        traverse _ None = pure None


-- SAMPLE REPL USAGES
-- ghci> import Data.Monoid
-- ghci> Some ( Sum  3) <> Some ( Sum 3)
-- ghci> Some (Sum {getSum = 6})
-- ghci> foldMap  id  $ Some "Tom"
-- ghci> foldMap  id  $ Some (Sum 12)
-- ghci> traverse id $ Some "Tom"
-- ghci> traverse id $ Some (Sum 12)

