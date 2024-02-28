{-# LANGUAGE LambdaCase #-}

module MonadIO where

import Prelude hiding (abs)
import Data.Bifunctor

class (Monad m) => MonadIO m where
  -- | "lifts" a computation from the 'IO' monad.
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO :: IO a -> IO a
  liftIO = id

-- ================================  IdentityT IO ========================================== --

newtype IdentityT m a =
  IdentityT {
    runIdentityT :: m a
  }

instance Functor m => Functor (IdentityT m) where
 fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
 fmap ab ma = ab <$> ma

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure = pure

  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (<*>) mab ma = mab <*> ma

instance (Monad m) => Monad (IdentityT m) where
  return :: Monad m => a -> IdentityT m a
  return =  pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (>>=) ma f = ma >>= f

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO :: IO a -> IdentityT m a
  liftIO a = IdentityT (liftIO a)

mkIdentityT1 :: IdentityT IO ()
mkIdentityT1 = liftIO $ putStrLn "Performing IO action"

mkIdentityT2 :: IdentityT IO String
mkIdentityT2 =
    liftIO $
        putStrLn "Performing IO action ..."
            >> pure "Done"

-- ================================  EitherT IO ========================================== --

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ fmap (fmap f) m

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ pure (Right x)
  (EitherT fab) <*> (EitherT m) = EitherT $ (<*>) <$> fab <*> m

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (EitherT m) >>= f = EitherT $ do
    v <- m
    case v of
      Left e  -> return (Left e)
      Right x -> runEitherT (f x)

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO a = EitherT $ liftIO $ fmap Right a

mkEitherT1 :: EitherT String IO ()
mkEitherT1 =
  liftIO $
    putStrLn "Performing IO action"

mkEitherT2 :: EitherT String IO String
mkEitherT2 =
  liftIO $
    putStrLn "Performing IO action ..."
      >> return "Done"

-- ================================  MaybeT IO ========================================== --

newtype MaybeT m a =
    MaybeT {
        runMaybeT :: m (Maybe a)
    }

instance (Functor m) => Functor (MaybeT  m) where
  fmap :: Functor m => (a -> b) -> MaybeT m a -> MaybeT m b
  fmap ab m = ab <$> m

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a =  MaybeT $ pure  (Just a)
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) (MaybeT mab) (MaybeT ma) =
    let fma = fmap (<*>) mab
        fmb = fma <*> ma
    in MaybeT fmb

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) fab =
    let mb = ma >>= \case Just a -> runMaybeT (fab a)
                          _      -> return Nothing
    in  MaybeT mb

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: MonadIO m => IO a -> MaybeT m a
  liftIO a = 
    let x = MaybeT (liftIO (fmap Just a))
    in x

mkMaybeT1 :: MaybeT IO ()
mkMaybeT1 =
  liftIO $
    putStrLn "Performing IO action"

mkMaybeT2 :: MaybeT IO String
mkMaybeT2 =
  liftIO $
    putStrLn "Performing IO action ..."
      >> return "Done"


-- ================================  ReaderT IO ========================================== --

newtype ReaderT r m a =
  ReaderT {
      runReaderT :: r -> m a
  }

instance (Functor m) => Functor (ReaderT r m ) where
  fmap :: Functor m => (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap ab (ReaderT rma) =
     ReaderT (
      let rmb = (fmap . fmap) ab rma
      in rmb
     )

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a =  ReaderT (\_ -> pure a)

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) (ReaderT mab) (ReaderT ma) =
    ReaderT (\r ->
      let ab = mab r
          a  = ma r
      in ab <*> a
    )

instance (Monad m) => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT rma) amb =
            ReaderT (\r ->
              rma r >>= (\a ->
                let ReaderT rmb = amb a
                in rmb r)
            )

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: MonadIO m => IO a -> ReaderT r m a
  liftIO x = ReaderT (\_ ->  liftIO x)


-- Example 1: runReaderT mkReaderT1 "example env"
mkReaderT1 :: ReaderT String IO ()
mkReaderT1 = do
  environment <- ReaderT pure
  liftIO $ putStrLn $ "Performing IO action with environment: " ++ environment

-- Example 2: runReaderT mkReaderT2 "example env"
mkReaderT2 :: ReaderT String IO String
mkReaderT2 = do
  environment <- ReaderT pure
  liftIO $ putStrLn $ "Performing IO action with environment: " ++ environment
  return "Done"


-- ================================  StateT IO ========================================== --

newtype StateT s m a =
  StateT {
      runStateT :: s -> m (a,s)
  }

instance (Functor m) => Functor (StateT s m ) where
  fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
  fmap ab (StateT sma) = StateT (fmap (first ab) . sma)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT (\s -> pure (a,s))

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT mab) (StateT ma) =
     StateT (\s -> do
          (ab, s') <- mab s
          (a, s'') <- ma s'
          let tpl = (ab a, s'')          
          pure tpl
     )

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT am) fam = 
    StateT (\s -> do
      tpl <- am s
      let (StateT x) = fam (fst tpl)
      x s
   )

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: MonadIO m => IO a -> StateT s m a
  liftIO a = StateT (\s ->  liftIO $ fmap (,s) a)

-- Example 1: runStateT mkStateT1 0
mkStateT1 ::  StateT Int IO ()
mkStateT1 = do
  stateT <- liftIO $ runStateT stateAction 0
  liftIO $ putStrLn $ "Performing IO action with state " ++ show stateT
  where 
    stateAction = StateT (\s -> pure ("a string value", s + 1))


-- Example 2:runStateT mkStateT2 0
mkStateT2 :: StateT Int IO String
mkStateT2 = do
  stateT <- liftIO $ runStateT stateAction 0
  liftIO $ putStrLn $ "Performing IO action with state " ++ show stateT
  pure "Done"
  where 
    stateAction = StateT (\s -> pure ("a string value", s + 1))


