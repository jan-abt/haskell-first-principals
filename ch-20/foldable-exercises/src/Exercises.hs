module
    Exercises
        where

import Data.Monoid

data Optional a = 
    None
    | Some a
    deriving (Show)

instance Foldable Optional 
    where 
        foldr :: (a -> b -> b) -> b -> Optional a -> b
        foldr _ z None = z
        foldr f z (Some x) = f x z
        -- foldr (*) 3 None
        -- foldr (*) 3 (Some (Sum 1.5) ) 
        -- foldr (++) "prepend here!" (Some "Ok, " ) 
        
        foldl :: (b -> a -> b) -> b -> Optional a -> b
        foldl _ z None = z
        foldl f z (Some x) = f z x
        -- foldl (*) 8 None
        -- foldl (*) 8 (Some (Sum 1.5) ) 
        -- foldl (++) "Append here" (Some ", ok?" ) 
        
        foldMap :: Monoid m => (a -> m) -> Optional a -> m
        foldMap _ None = mempty 
        foldMap f (Some a) = f a
         -- foldMap (+ 5)  (Some (Sum 1) ) 
         -- foldMap (+ 5 )  (None :: Optional (Sum Int)) 
         -- foldMap (++ "prepend to me")  (Some "Ok, ")
         -- foldMap (++ "prepend to me")  None

{-
  implementing functions in terms of foldMap or foldr from Foldable  
-}

sum' :: (Foldable t, Num a) => t a -> a
sum' t   = getSum $ foldMap Sum t

sum'' :: (Foldable t, Num a) => t a -> a
sum'' t   = foldr (\e a -> getSum $ (Sum e) <> (Sum a) ) 0 t

product' :: (Foldable t, Num a) => t a -> a
product' t  = getProduct $ foldMap Product t

product'' :: (Foldable t, Num a) => t a -> a
product'' t  = foldr (\e a -> getProduct $ (Product e) <> (Product a)) 1 t

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e t =  
    getAny $ 
        foldr (\n a -> Any (n == e) <> a ) mempty t

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' e t =  
    getAny $ 
        foldMap (\n -> Any (n == e) )  t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t = 
    foldr (\e a -> 
        case a of 
        Nothing -> Just e  
        _ -> case (Just (e <) <*> a) of
             Just True -> Just e   
             _         -> a
    ) Nothing t

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' t = 
     foldr (\e a -> 
        case a of 
        Nothing -> Just e  
        _ -> case (Just (e >) <*> a) of
             Just True -> Just e   
             _         -> a
    ) Nothing t

null'' :: (Foldable t) => t a -> Bool
null'' t = 0 == (getSum $ foldr (\_ _ -> Sum 1 ) mempty t :: Integer )

null' :: (Foldable t) => t a -> Bool
null' t = 0 == ( getSum  $ foldMap (\_ -> (Sum 1) ) t :: Integer )

length' :: (Foldable t) => t a -> Int
length' t = foldr (\_ a -> a + 1 ) 0 t

length'' :: (Foldable t) => t a -> Int
length'' t = getSum $ (foldMap (\_ -> (Sum 1) ) t)

toList' :: (Foldable t) => t a -> [a]    
toList' t = foldr (\e a -> e:a ) [] t

-- use foldMap.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' t = foldMap id t

-- define foldMap'' in terms of foldr.
foldMap'' :: (Foldable t, Monoid m) => (a  -> m) -> t a -> m   
foldMap'' f t =  foldr (\e a -> (f e) <> a ) mempty  t

data Constant a b = 
    Constant a   
    deriving (Show)

instance Foldable (Constant a ) where
    foldr _ acc _ = acc
    foldMap _ _ = mempty

-- foldr (+) (Sum 2) (Constant $ Sum 34)        

data Two a b = 
    Two a b     
    deriving (Show)

instance Foldable (Two a) 
    where 
        foldr f ac (Two _ b) = f b ac
        foldMap f (Two _ b) = f b
        
-- foldMap (+ 5)  (Two  (Sum 1) (Sum 2)) 
-- foldMap (\n -> (n * 5)) (Two (Product 0) (Product 4))       
-- foldr (+) (Sum 1)  (Two  (Sum 1) (Sum 2))      

data Three a b c = Three a b c  

instance Foldable (Three a b) 
    where 
        foldr f ac (Three _ _ c) = f c ac
        foldMap f (Three _ _ c) = f c   
-- foldMap (+ 5)  (Three  ("Jan") (Just 2) (Sum 3))               
        
data Three' a b = 
    Three' a b b    
    deriving (Show)

instance Foldable (Three' a) 
    where 
        foldr f acc (Three' _ b b') =   f  b  (f  b'  acc)
        foldMap f (Three' _ b b') = f b <> f b'
-- foldMap (+ 5)  (Three'  ("Jan") (Sum 2) (Sum 3))             

data Four' a b = Four' a b b b    

instance Foldable (Four' a) 
    where 
        foldr f acc (Four' _ b b' b'') =   f  b  (f  b'  (f  b''  acc))
        foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''
-- foldMap (+ 5)  (Four'  ("Jan") (Sum 2) (Sum 3) (Sum 4))         

-- filterF (\n -> (mod n 2) == 0 ) [1..10]
-- filterF (const True) (1, (Sum 1)) :: (Sum Int, (Sum Int))
filterF :: (  Applicative f, Monoid (f a), Foldable f) => (a -> Bool) -> f a -> f a
filterF p t = foldMap (\el -> applyPredicate p el) t
    where
        applyPredicate :: (Applicative f, Monoid (f a)) => (a-> Bool) -> a -> f a
        applyPredicate f e = 
            case (f e) of
            True -> pure e
            _    -> mempty


    --foldMap (\e -> case (f e) of True ->  pure e;_    -> mempty ) [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]