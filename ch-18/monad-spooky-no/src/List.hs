module
    List    
        where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data List a = 
    Nil
  | C a (List a)    
  deriving (Eq, Show)
         
-- Nil <> (C 2 (C 1 Nil))
-- (C 2 (C 1 Nil)) <> Nil 
-- (C 3 (C 2 Nil)) <> (C 1 Nil) <>  Nil
-- Nil <> (C 1 Nil) <> (C 3 (C 2 Nil))
-- (C 2 (C 1 Nil)) <> (C 5 (C 4 (C 3 Nil))) 
-- (C 5 (C 4 (C 3 Nil))) <> (C 2 (C 1 Nil))
-- Nil <> (C 3 (C 2 Nil)) <> (C 1 Nil) <> Nil <> (C 4 (C 5 Nil)) 
instance Semigroup (List a)
    where       
        (<>)  xs Nil = xs
        (<>)  Nil ys = ys
        (<>) (C x Nil) xs = (C x xs)
        (<>) (C x xs) ys = 
            let (C x' xs') = xs                               
            in  (C x ((<>) (C x' xs') ys))

instance Semigroup a => Monoid (List a)
    where
        mempty = Nil
        mappend = (<>)

-- fmap (*5) (C 3 (C 2 (C 1 Nil)))
-- fmap (*5) [3,2,1]
instance Functor List
    where
        fmap _ Nil = Nil
        fmap f (C x xs) = C (f x) ( fmap f xs )

-- pure (*5) :: List (Int -> Int)   
-- pure (*5) :: [] (Int -> Int)   
-- pure (*5) <*> (C 3 (C 2 (C 1 Nil)))
-- pure (*5) <*> [3,2,1]
-- (C (+) (C (-) Nil)) <*> (C 2 (C 3 Nil)) 
-- [(+), (-)] <*> [2,3] 
-- (C (+) (C (-) Nil)) <*> (C 2 Nil) <*> (C 3 Nil)
-- [(+), (-)] <*> [2] <*> [3]
instance Applicative List
    where
        pure x = (C x Nil)
        (<*>)  Nil _ = Nil
        (<*>) (C f Nil) xs = fmap f xs 
        (<*>) (C f (C f' fs')) xs = (fmap f xs) <> ((<*>) (C f' fs') xs) 
     
--  Nil >>= (\n -> C (1+n) Nil)     
--  (C 0 Nil) >>= (\n -> C (1+n) Nil) 
--  [0] >>= (\n -> [1 + n]) 
--  (C 0 (C 1 Nil) ) >>= (\n -> C (1+n) Nil) 
--  [0, 1] >>= (\n -> [1 + n]) 
--  (C 0 (C 1 ( C 2 Nil) ) ) >>= (\n -> C (1+n) Nil) >>= (\n -> C (1+n) Nil)
--  [0, 1, 2] >>= (\n -> [1 + n]) >>= (\n -> [1 + n])
instance Monad List            
    where
        return = pure
        (>>=) Nil _ = Nil
        (>>=)(C x Nil) f = f x
        (>>=)(C x xs') f = f x <> ((>>=) xs' f)

-- generates lists like this one: (C 26 (C (-41) (C 47 (C 23 (C (-52) Nil)))))
instance (Arbitrary a) => Arbitrary (List a) 
    where 
        arbitrary = -- Gen [List a]
            let genElems = vectorOf 5 (pure <$> arbitrary) 
            in  genElems >>= (\elems -> return (combine elems)) 
                where 
                    combine :: [List a] -> List a 
                    combine [] = Nil
                    combine (l:ls) = l <> (combine ls)              
            
instance (Eq a) => EqProp (List a) 
    where
        (=-=) = eq

listTest :: IO ()    
listTest = do
    let trigger = undefined :: List (Int, Int, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger                    

-- arrayTest :: IO ()                
-- arrayTest = do
--     let trigger = undefined :: [] (Int, Int, Int)
--     quickBatch $ functor trigger
--     quickBatch $ applicative trigger
--     quickBatch $ monad trigger          
                 