{-# LANGUAGE LambdaCase #-}

module EitherTransformer where


{-
  "EitherT" is a monad transformer for combining the effects of monad "m" with those of the "Either" monad.
  "l" is the type of the value contained in the Either monad's "Left" data constructor. 
  "m" is any monad such as for example IO, State, Reader, etc.
  "r" is the type of the value contained in the Either monad's "Right" data constructor. 
  The purpose of "EitherT" is to lift the "Either" monad into the monad "m", thus combining their effects
-}

-- ================================ Data Type ======================================== -- 

newtype EitherT l m r =
    EitherT { runEitherT :: m (Either l r)
}
-- ghci> import Data.Functor.Identity

-- ghci> runEitherT $ EitherT ( Identity (Right 42) )

-- ================================ Functor ======================================== -- 

-- Functor has kind (* -> *) meaning it only accepts types with one free type variable.
-- Therefore, Left "l" and the outer mondad "m" have to be fixed to the structure, 
-- leaving only Right "a" to be available for mapping operations    
instance  (Functor m) => Functor (EitherT l m )
    where
        fmap :: Functor m => (a -> b) -> EitherT l m a -> EitherT l m b
        fmap fab (EitherT me) =  EitherT $ (fmap . fmap ) fab me

-- ghci> import Data.Functor.Identity

-- ghci> let etr =  EitherT ( Identity (Right 45))        
-- ghci> runEitherT $ fmap (+5) etr       

-- ghci> let etl =  EitherT ( Identity (Left "Some Error happened"))        
-- ghci> runEitherT $ fmap (+5) etl       

-- ================================ Applicative ======================================== -- 

-- Applicative has kind (* -> *) meaning it only accepts types with one free type variable.
-- Therefore, Left "l" and the outer mondad "m" have to be fixed to the structure, 
-- leaving only Right "a" to be available for mapping operations    
instance (Applicative m) => Applicative (EitherT l m)
    where
        -- looking at the regular results when calling "pure" for the Either type
        -- pure 5 :: (Either String Int)
        -- we get: Right 5
        pure :: Applicative m => a -> EitherT l m a
        -- so we can do similarly ...
        pure a = EitherT (pure (Right a))
     -- Since there isn't  an instance for ‘Show (EitherT String Identity Int), 
     -- verify "pure" functionality by getting the wrapped value inside of EitherT instead

     -- ghci> import Data.Functor.Identity
     
     -- ghci> let p = pure 5 :: EitherT String Identity Int
     -- ghci> runEitherT p

     -- (<*>) ::                            f (a -> b) ->           f a ->           f b 
        (<*>) :: Applicative m => EitherT l m (a -> b) -> EitherT l m a -> EitherT l m b
        (<*>) (EitherT fab) (EitherT fb) = 
         let fab' = (<*>) <$> fab
             fb' = fab' <*> fb 
         in EitherT fb'
    -- ghci> import Data.Functor.Identity
    
    -- ghci> let fab =  EitherT ( Identity (Right (+12)))     
    -- ghci> let fa =  EitherT ( Identity (Right (20)))  
    -- ghci> runEitherT $ fab <*> fa    

    -- ghci> let fab =  EitherT ( Identity (Right (+12)))     
    -- ghci> let fe =  EitherT ( Identity (Left ("Some Error")))     
    -- ghci> runEitherT $ fab <*> fe      
    
    -- ghci> let fab = pure (+5) :: EitherT String Identity (Int -> Int)
    -- ghci> let fa =  EitherT ( Identity (Right (20)))  
    -- ghci> runEitherT $ fab <*> fa      



-- ================================ Monad ======================================== -- 


instance (Monad m) => Monad (EitherT l m)
    where
        return :: Monad m => r -> EitherT l m r
        return = pure
     -- 
        (>>=) :: Monad m => EitherT l m r -> (r -> EitherT l m b) -> EitherT l m b
        (>>=) (EitherT m) ab =  EitherT mTransformer
                where 
                    mTransformer = 
                       m >>= \case
                           Left l ->   pure (Left  l)
                           Right r ->  runEitherT $ ab r
     -- ghci> import Data.Functor.Identity

     -- ghci> let ab = \a ->  EitherT ( Identity (Right (* a)) ) 
     -- ghci> let m =  EitherT ( Identity (Right (20))) 
     -- ghci> runEitherT $ m >>= fab


-- ================================ Exercises ======================================== -- 

swapEitherT :: (Functor m) => EitherT l m r -> EitherT r m l 
swapEitherT (EitherT lmr ) = 
  let swapped = fmap (swapEither Left Right) lmr
  in EitherT swapped
  where 
   -- a.k.a catamorphism, which is a way to break down a data structure into
   -- a simpler representation or value. 
   -- Here for Either a b, we could define our catamorphism by providing two functions (one for each side of the Either). 
   swapEither :: ( l -> Either l r ) -> ( r -> Either l r) -> Either r l -> Either l r 
   swapEither l _ (Right r) = l r
   swapEither _ r (Left l) =  r l

-- ghci> import Data.Functor.Identity

-- ghci> let l =  EitherT ( Identity (Left (20))) 
-- ghci> runEitherT $ swapEitherT l

-- transformer variant of the either catamorphism.
eitherT :: Monad m => (l -> m c ) -> (r -> m c) -> EitherT l m r -> m c
eitherT lmc rmc (EitherT lmr)  = 
   lmr >>= 
        \case
            Left l -> lmc l
            Right r -> rmc r

-- ghci> import Data.Functor.Identity

-- ghci> let lmc =  Identity . Left 
-- ghci> let rmc =  Identity . Right 
-- ghci> let lmr =  EitherT ( Identity (Right (20))) 
-- ghci> eitherT lmc rmc  lmr

-- ghci> let lmr =  EitherT ( Identity (Left ("error"))) 
-- ghci> eitherT (Identity . Left ) (Identity . Right ) lmr    