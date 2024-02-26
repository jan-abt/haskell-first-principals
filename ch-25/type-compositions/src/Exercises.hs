module 
    Exercises
        where

import Data.Monoid

newtype Composable f g arg = 
    Composable { getComposable :: f (g arg) } 
    deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Composable f g )
     where 
        fmap :: (a -> b) -> Composable f g a -> Composable f g b
        fmap f  (Composable fga ) = Composable $ (fmap . fmap) f  fga

instance (Applicative f,  Applicative g) => Applicative (Composable f g)
    where
        pure :: a -> Composable f g a
        pure a =  Composable (pure (pure a))

        (<*>) :: Composable f g (a -> b) -> Composable f g a -> Composable f g b
        Composable fgab <*> Composable fga = 
            let partiallyAppliedStarLifted = fmap (<*>) fgab  
            in Composable $ partiallyAppliedStarLifted <*> fga
 -- for short: Composable $      (<*>) <$> fgab        <*> fga

instance (Foldable f, Foldable g) => Foldable (Composable f g) 
    where
        foldMap :: Monoid m => (arg -> m) -> Composable f g arg -> m
        foldMap f (Composable fga) = foldMap (foldMap f) fga
                                     
instance (Traversable f, Traversable g) => Traversable (Composable f g) 
    where
        traverse :: (Traversable f, Traversable g, Applicative f1) => 
            (a -> f1 b) -> Composable f g a -> f1 (Composable f g b)
        traverse f (Composable fga) = Composable <$> traverse (traverse f) fga
 
main :: IO () 
main = do
   
   print $ (*5) <$> Composable (Just [2])
   print $ Composable (Just [(*5)]) <*> Composable (Just [2])
   print (foldMap (* 5) (Composable (Just [2])) :: (Sum Int))
   
   n <-  traverse (\x -> pure (x * 5)) (Composable (Just [2]) ) :: IO (Composable Maybe [] Integer)                 
   print n
   
