{-# LANGUAGE NoImplicitPrelude #-}

module
    FunctionReader
            where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a 

const :: a -> b -> a
const a _ = a

(.) :: (b -> c) -> (a -> b) -> (a -> c) 
f . g = \a -> f (g a)        

------------------------------------------
class Functor f 
    where
        fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f 
    where 
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f 
    where 
        return :: a -> f a
        (>>=) :: f a -> (a -> f b) -> f b 
------------------------------------------
-- the functorial structure in this instance is a partially applied function,
-- whose input argument type is fixed, such as in for examplpe (Int -> a), (String -> a) or similar
instance Functor ((->) r) 
    where
        fmap f g = f . g

instance Applicative ((->) r) 
    where
        pure = const
        rab <*> ra = \r -> rab r (ra r)    
     -- OR --
     -- rab <*> ra = \r -> 
     --     let func = (fmap (\ab -> (ab . ra) r ) rab )   --
     --     in func r                

instance Monad ((->) r) 
    where 
        return = pure
        ra >>= arb = \r -> (arb . ra ) r r 
     -- OR -- 
     -- ra >>= arb = flip arb <*> ra      