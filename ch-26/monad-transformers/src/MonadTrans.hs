
module MonadTrans where

{-

    MonadTrans is a type class that represents a transformer of monads. 
    Monad transformers allow you to combine multiple monads to create a 
    new monad that incorporates the effects of each individual monad.

    The MonadTrans type class provides a single method, lift, 
    which is used to lift a computation from the inner monad to the combined monad. 
        
-}

class MonadTrans t where
    -- The lift function has the following type signature:    
    lift :: Monad m => m a -> t m a

-- =========================================================================== --


newtype EitherT l m r =
    EitherT { runEitherT :: m (Either l r)
}

instance MonadTrans ( EitherT e)
    where
        lift :: Monad m => m a -> EitherT e m a
        lift ma = EitherT $ Right <$> ma

mkEitherT1 :: EitherT String IO ()
mkEitherT1 =  lift $ putStrLn "Performing IO action" 

mkEitherT2 :: EitherT Int IO String
mkEitherT2 =
    lift $
        putStrLn "Performing IO action ..." 
            >> pure "Done"

-- runEitherT mkEitherT1 
-- runEitherT mkEitherT2 

-- =========================================================================== --

newtype ReaderT r m a =
    ReaderT {
       runReaderT :: r -> m a
    }

instance MonadTrans ( ReaderT r)
    where
        lift :: Monad m => m a -> ReaderT r m a
        -- lift ma = ReaderT (\_-> ma)
        -- or ..
        -- lift ma = ReaderT (const ma)
        -- or ..
        lift = ReaderT . const

mkReaderT1 :: ReaderT Int IO ()
mkReaderT1 =  lift $ putStrLn "Performing IO action" 

mkReaderT2 :: ReaderT Int IO String
mkReaderT2 =
    lift $
        putStrLn "Performing IO action ..." 
            >> pure "Done"       

-- runReaderT mkReaderT1 0
-- runReaderT mkReaderT2 0

-- =========================================================================== --

newtype StateT s m a =
    StateT {
        runStateT :: s -> m (a,s)
    }

instance MonadTrans (StateT r)
    where
        lift :: Monad m => m a -> StateT s m a    
        lift ma = StateT (\s -> (,s) <$> ma)

mkStateT1 :: StateT Int IO ()
mkStateT1 =  lift $ putStrLn "Performing IO action" 

mkStateT2 :: StateT Int IO String
mkStateT2 =
    lift $
        putStrLn "Performing IO action ..." 
            >> pure "Done"       
 
-- runStateT mkStateT1 0
-- runStateT mkStateT2 0 >>= \(v,s ) -> pure (v, s+1)

-- =========================================================================== --
