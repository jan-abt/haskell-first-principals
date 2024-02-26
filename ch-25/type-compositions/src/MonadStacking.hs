{-# LANGUAGE InstanceSigs #-}
module
    MonadStacking
        where

-- =================== Data Types ======================= --

-- Amulating the familiar Data.Functor.Identity data type. 
newtype Id a =
    Id { runID :: a } 
    deriving (Eq, Show)

-- The Id Monad Transformer type, serving only to
-- to specify that additional structure should exist. 
newtype IdTransformer f a =
    IdTransformer { runIDT :: f a } 
    deriving (Eq, Show)      

-- =================== Functors ======================= --    

instance Functor Id
    where
        fmap :: (a -> b) -> Id a -> Id b
        fmap f (Id a) = Id (f a)

instance (Functor m) => Functor (IdTransformer m) 
    where 
        fmap :: (a -> b) -> IdTransformer m a -> IdTransformer m b
        fmap f (IdTransformer fa) = IdTransformer (fmap f fa)      

-- =================== Applicatives ======================= --    

instance Applicative Id 
    where 
        pure :: a -> Id a
        pure = Id
        
        (<*>) :: Id (a -> b) -> Id a -> Id b
        (<*>) (Id f) (Id a) = Id (f a)

instance (Applicative m) => Applicative (IdTransformer m) 
    where
        pure :: Applicative m => a -> IdTransformer m a
        pure x = IdTransformer (pure x) 
        
        (<*>) :: Applicative m => IdTransformer m (a -> b) -> IdTransformer m a -> IdTransformer m b
        (<*>) (IdTransformer fab) (IdTransformer fa) = IdTransformer (fab <*> fa)

-- =================== Monads ======================= --    

instance Monad Id 
    where 
        return :: a -> Id a
        return = pure
        
        (>>=) ::  Id a -> (a -> Id b) -> Id b
        (>>=) (Id a)  f = f a


instance (Monad m) => Monad (IdTransformer m) 
    where 
        return :: Monad m => a -> IdTransformer m a
        return = pure

        (>>=) :: IdTransformer m a -> (a -> IdTransformer m b) -> IdTransformer m b 
        (>>=) (IdTransformer ma) f =
                    let aimb = runIDT =<< fmap f ma
                    in IdTransformer aimb


-- =================== Main ======================= --    

main :: IO () 
main = do
   putStrLn "-- ========================================== --"
