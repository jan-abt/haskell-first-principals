{-# LANGUAGE LambdaCase #-}

module MaybeTransformer where


{-
  "MaybeT" is a monad transformer for combining the effects of an 
  underlying monad "m" with those of the "Maybe" monad.
  "m" can be any monad such as IO, State, Reader, etc.
  "a" is the type of the value the Maybe monad wraps. 
  The purpose of "MaybeT" is to 
  lift the "Maybe" monad into the underlying monad "m", 
  thus combining their effects.
-}

-- ================================ Data Type ======================================== -- 


newtype MaybeT m a =
    MaybeT {
        runMaybeT :: m (Maybe a)
    }

-- ================================ Functor ======================================== -- 


instance (Functor m) => Functor (MaybeT m)
    where
        fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

-- ================================ Applicative ======================================== -- 


instance (Applicative m) => Applicative (MaybeT m)
    where
        -- looking at the regular results when calling "pure" for the Maybe type
        -- pure 5 :: (Maybe Int)
        -- we get: Just 5
        pure :: Applicative m => a -> MaybeT m a
        -- so we can do similarly ...
        pure a =  MaybeT (pure (Just a))
     -- Since there isn't  an instance for ‘Show (MaybeT Identity Int), 
     -- verify "pure" functionality by getting the wrapped value inside of MaybeT instead
     -- ghci> let p = pure 5 :: MaybeT Identity Int
     -- ghci> runMaybeT p 

     -- (<*>) ::                         f (a -> b) ->        f a ->        f b 
        (<*>) :: Applicative m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
        (<*>) (MaybeT mmab) (MaybeT mma) =
                MaybeT $ mMaMb mmab <*> mma
                where
                    mMaMb:: m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
                    mMaMb =  fmap (\inner -> (inner <*>))
     -- for short: MaybeT $ fmap (<*>) mmab <*> mma


-- ================================ Monad ======================================== -- 


instance (Monad m) => Monad (MaybeT m)
    where
        return :: Monad m => a -> MaybeT m a
        return = pure

        (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
        (>>=) (MaybeT ma) ammb =             
            let action = 
                  ma >>= \case 
                    -- apply function "ammb", which returns "MaybeT m b",
                    -- and apply "runMaybeT" to it to wrap it into the "m" context
                    (Just a) -> runMaybeT (ammb a) 
                    Nothing -> pure Nothing
            in MaybeT action
              





