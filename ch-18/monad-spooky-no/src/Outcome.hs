module 
    Outcome
        where

import Control.Monad (join)    

{-

    We can’t make a Monad failure Validation that accumulates the errors like the Applicative does. 
    Instead, any Monad instance would for Validation be identical to the Either’s monad instance.
    
-}

data Outcome a b = 
    Failure a
    | Success b
    deriving (Eq, Show)

instance Functor (Outcome a) 
    where 
        fmap _ (Failure  a) = Failure a
        fmap f (Success b) = Success (f b)

instance Applicative (Outcome a) 
    where
        pure                 = Success 
        (<*>) (Success f) b  = fmap f b
        (<*>) (Failure a) _ = Failure a
--  pure (2+) <*> (Success 12)

instance Monad (Outcome a) 
    where 
        return = pure
        (>>=)  (Failure a) _ = Failure a
        (>>=) (Success b) f = f b

--  Success 12 >>= (\x -> Success (2 + x))
--  return 12 >>= (\x -> Success (2 + x))
--  Failure "Aborted" >>= (\_ -> Success (2 + undefined))
--  Success 12 >>= (\x -> if x >= 12 then Success (2 + x) else Failure "Too small")