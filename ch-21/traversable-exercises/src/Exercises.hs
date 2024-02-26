module
    Exercises
        where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Traversable


{-

    -- fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b

    -- flip-bind
(=<<) :: Monad m => (a -> m b) -> m a -> m b

    -- traverse
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)    

 similar to flip-bind, traverse is itself generating more structure. 
 However, unlike flip-bind, that structure can be of a different type 
 than the structure we lifted over to apply the function. 
 And at the end, as did sequence, it will flip the two structures around.    

-}    

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA  $ Just (Sum 1)
-- Sum {getSum = Just 1}
-- sequenceA  $ fmap Just [1..5]
-- Just [1,2,3,4,5]

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)    
-- traverse (\n -> 5*n) $  Just (Sum 1)
-- Sum {getSum = Just 5}

-- ==================================================================

-- -- a traversable instance for Identity
-- newtype Identity a = 
--     Identity a
--     deriving (Eq, Ord, Show)

-- instance Functor Identity
--     where
--         fmap f (Identity a)  = Identity (f a)

-- instance Foldable Identity
--     where
--         foldMap f (Identity a) = f a
--         -- foldMap (+2) $ Identity (Sum 1)
--         foldr f acc (Identity a) = f a acc
--         --  foldr (\n a ->  (n+a)) 0 $ Identity 1

-- instance Traversable Identity 
--     where
--         traverse f (Identity a) = fmap Identity (f a)        
--         -- traverse (\(Just d)-> Just d) $ Identity (Just 1)

-- ==================================================================        

newtype Constant a b =
    Constant { 
            getConstant :: a 
    }
    deriving (Eq, Ord, Show)

instance Functor (Constant a)
    where
        fmap _ (Constant a)  = (Constant a)

instance Foldable (Constant a)
    where
       foldMap _ (Constant _) = mempty
       foldr _ z (Constant _) = z

instance Traversable (Constant a) 
    where
        traverse _ (Constant a) = pure (Constant a)

-- ==================================================================    


data Optional a = 
    Nada
    | Yep a    
    deriving (Show)

instance Functor Optional
    where
        fmap _ Nada = Nada
        fmap f (Yep a) = Yep (f a)

instance Foldable Optional
    where
        foldMap _ Nada = mempty
        foldMap f (Yep a) = f a

instance Applicative Optional
    where
        pure = Yep 
        (<*>) Nada _ =  Nada
        (<*>) (Yep f) o =  fmap f o

instance Traversable Optional
    where
        -- traverse f Nada = fmap (\_ -> Nada) (pure f)
        traverse f Nada = fmap (\_ -> Nada) (pure f)
        traverse f (Yep a) = fmap Yep (f a)                        
      -- traverse (\n -> fmap (+2) n) $ Just (Sum 1)
      -- traverse (\n -> fmap (+2) n) $ Yep (Sum 1)

-- ==================================================================        

data List a = 
    Nil
    | Cons a (List a)    
    deriving (Eq, Ord, Show)

instance Functor List
    where
        fmap _ Nil = Nil
        fmap f' (Cons a cs) = Cons (f' a) (fmap f' cs)
     -- fmap (*5) (Cons 3 (Cons 2 (Cons 1 Nil)))

instance Semigroup (List a)
   where       
        (<>)  Nil cs' = cs'
        (<>) (Cons c Nil) cs' = (Cons c cs')
        (<>) (Cons c cs) cs' = (Cons c ((<>) cs cs'))                

instance (Semigroup a ) => Monoid (List a)
    where
        mempty = Nil
        mappend = (<>)

instance Applicative List
    where
        pure a = Cons a Nil 
        (<*>)  Nil _ = Nil
        (<*>) (Cons f fs)  ls =  (fmap f ls) <> (fs <*> ls)
        -- (Cons (*2) (Cons (*4) Nil)) <*>  (Cons 2 (Cons 4 Nil))

instance Foldable List
    where
        foldMap _ Nil = mempty 
        foldMap f (Cons c cs) =  (f c) <> (foldMap f cs)
        
-- constructs a new List from a given one, using foldr, while also applying a function to each element
instance Traversable List 
    where 

        -- foldr (\n a -> (liftA2 (+) n a)) (Just 0) (Cons (Just 5) (Cons (Just 3) Nil) )
        -- ghci>Just 8
        -- foldr (\e a -> (liftA2 (Cons) e a)) (pure Nil) (Cons (Just 5) (Cons (Just 3) Nil))        
        -- ghci>Just (Cons 5 (Cons 3 Nil))
        traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
                                                               -- Nil :: List a
                                                          -- pure Nil :: Applicative f => f (List a)
        traverse f cs = foldr (\e l -> liftA2 Cons (f e) l) (pure Nil) cs
     
        -----------------------------------------------------------------
        --------------------------- SAME THING --------------------------
        -----------------------------------------------------------------
        -- traverse f cs = foldr eachUsing (pure Nil) cs
        --     where                
        --         eachUsing e l =  liftA2    (Cons)              (f e)         l
        --         eachUsing e l =  pure      (Cons)      <*>     (f e)    <*>  l
        --     --  Applicative f =>   f (a -> List a -> List a)  (a -> f b)  (List a) 
        
        -- traverse id (Cons (Right 3) (Cons (Right 4) Nil))
        -- traverse id (Cons (Just 1) (Cons (Just 2) (Cons (Just 3) Nil)))    
            
-- ==================================================================        

data Three a b c = 
    Three a b c
    deriving (Eq, Ord, Show)

instance Functor (Three a b)
    where
        fmap f (Three a b c) = Three a b (f c)

instance (Semigroup a,Semigroup b, Semigroup c) => Semigroup (Three a b c)
    where
        (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c)        
    where
        mempty = Three mempty mempty mempty
        mappend = (<>)

instance ( Monoid a, Monoid b) => Applicative  (Three a b)
    where
        pure c =  Three mempty mempty c 
        -- pure 5 :: (Three String  (Sum Int)  Int)
        -- ghci> Three "" (Sum {getSum = 0}) 5
        (<*>) (Three _ _  f) t =  fmap f t 
        -- (pure (+2)) <*> (Three "Jan" (Sum 52) 4)
        -- ghci> Three "Jan" (Sum {getSum = 52}) 6
        
instance Foldable (Three a b)
    where
        foldMap f (Three _ _  c) = f c
        -- foldMap (\n -> n <> Product 2) (Three "Jan" True (Product 5))
        -- ghci> Product {getProduct = 10}      

instance Traversable (Three a b)
    where
     -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        traverse f (Three a b c) =  fmap (Three a b )(f c)
        -- traverse (\e ->  Right (2 * (getSum e)) ) (Three "Jan" 42 (Sum 5))
        -- Right (Three "Jan" 42 10)
        -- the function type (a -> f b) here means that the outgoing type,
        -- resulting from function application CAN be different. It doesn't have to be:
        -- traverse id (Three "Jan" 42 (Sum 12))
        -- Sum {getSum = Three "Jan" 42 12}
        -- traverse (\e ->  (*2) <$> e ) (Three "Jan" 42 (Sum 5))
        -- Sum {getSum = Three "Jan" 42 10}
-- ==================================================================        

data Three' a b = 
    Three' a b b
    deriving (Eq, Ord, Show)    

instance Functor (Three' a)
   where
       fmap f (Three' a b b') = Three' a (f b) (f b')

instance  ( Monoid a) => Applicative  (Three' a)
   where
       pure n = (Three' mempty n n) 
       (<*>) (Three' _ _ f) (Three' a b b') =  (Three' a (f b) (f b'))
       
instance Foldable (Three' a)
   where
       foldMap f (Three' _ b b') = (f b) <> (f b')

instance Traversable (Three' a)
   where
       traverse f (Three' a b b') =  liftA2 (Three' a) (f b) (f b')
    -- traverse (\e ->  Right (2 * (getSum e)) ) (Three' "Jan" (Sum 10) (Sum 5)) 

-- ==================================================================        

data S n a = 
    S (n a) a -- S (Just 42) 10   
    deriving (Eq, Ord, Show)    
-- example :: Num a => S Maybe a
-- example = S (Just 42) 10

instance (Functor n) => Functor (S n)
    where
        fmap f (S n a ) = S (fmap f n) (f a)

instance (Functor n, Foldable n) =>  Foldable (S n) 
    where 
     foldMap f (S n a ) = (foldr (\e a -> e <> a) mempty (fmap f n)  ) <> (f a)
-- foldMap (+2) $ S (Just ( Sum (42)))  (Sum (10))     
-- Sum {getSum = 56}

instance (Traversable n) => Traversable (S n)
    where 
        traverse f (S n a) =  let sn' = traverse f n
                                  sa' =  f a
                              in liftA2 S sn' sa'
-- traverse id (S (Just (Sum 42)) (Sum 10))
-- ghci> Sum {getSum = S (Just 42) 10}
-- ==================================================================        

data Tree a = 
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a) 
    deriving (Eq, Show)

instance Functor Tree 
    where 
        fmap _ Empty = Empty
        fmap f (Leaf a) = Leaf (f a)
        fmap f (Node e n r) = Node (fmap f e) (f n) (fmap f r)
-- fmap (*5) $ Node (Leaf (Sum 1)) (Sum 2) (Node (Leaf (Sum 3)) (Sum 4) Empty)

instance Foldable Tree 
    where
        foldMap _ Empty = mempty
        foldMap f (Leaf a) = f a
        foldMap f (Node e n r) =  (foldMap f e) <> (f n) <> (foldMap f r)
-- foldMap (+2) $ Node (Leaf (Sum 1)) (Sum 2) (Node (Leaf (Sum 3)) (Sum 4) Empty)


instance Traversable Tree 
    where
        traverse _ Empty =  pure Empty
        traverse f (Leaf a) = fmap Leaf (f a)
        traverse f (Node e n r) =  pure Node  <*> (traverse f e) <*> (f n) <*> (traverse f r)

-- traverse id $ Node (Leaf (Sum 1)) (Sum 2) (Node (Leaf (Sum 3)) (Sum 4) Empty)        