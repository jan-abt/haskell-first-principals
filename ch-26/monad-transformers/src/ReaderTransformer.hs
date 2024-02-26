
module ReaderTransformer where


{-

   ReaderT is a monad transformer , which means that it is a type that 
   transforms one monad into another by adding additional behavior.
   The ReaderT monad transformer is commonly used to thread  a shared environment or configuration,  
   through a computation without explicitly passing it as a parameter.
 
  `r` is the type of the read-only configuration or environment made available to the underlying monad. 
  `m` is the type of the underlying monad and can be any such monad as for example IO, State, Reader, etc.
  `a` is the type of the computation result.

-}

-- ================================ Data Type ======================================== -- 

newtype ReaderT r m a =
    ReaderT {   
        -- "runReaderT" is a field accessor. 
        -- It is not explicitly implemented since it is generated by the "ReaderT" newtype. 
        -- It has the type: 
        -- runReaderT :: ReaderT r m a -> r -> m a                
        runReaderT :: r -> m a 
    }

-- ghci> import Data.Functor.Identity
-- ghci> let rt = ReaderT ( Identity . (+12) ) 
-- ghci> runReaderT rt 5

-- ghci> import Data.Monoid
-- ghci> let rt = ReaderT ( Identity . Sum) 
-- ghci> runReaderT rt 5

-- ================================ Functor ======================================== -- 

-- Functor has kind (* -> *) meaning it only accepts types with one free type variable.
-- Therefore, "r" and "m", the type of the underlying mondad, have to be fixed to the structure.
instance  (Functor m) => Functor (ReaderT r m )
    where
        fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
        fmap ab (ReaderT rma) =  ReaderT $ (fmap . fmap ) ab rma

-- ghci> import Data.Functor.Identity

-- ghci> let etr = ReaderT ( Identity . (+ 45))     
-- ghci> let rt = fmap (+5) etr     
-- ghci> runIdentity $ runReaderT rt 5

-- ================================ Applicative ======================================== -- 

-- Functor has kind (* -> *) meaning it only accepts types with one free type variable.
-- Therefore, "r" and "m", the type of the underlying mondad, have to be fixed to the structure.
instance (Applicative m) => Applicative (ReaderT r m)
    where
        pure :: Applicative m => a -> ReaderT l m a
        --Note that the type of ReaderT itself is a function.
        -- "pure", ignores the environment and just returns the given value.             
        pure a =  ReaderT $ \_ -> pure a

     -- ghci> import Data.Functor.Identity

     -- ghci> let computaton = pure 42 :: ReaderT String Identity Int 
     -- ghci> let env = "My Environment"  
     -- ghci> runIdentity $ runReaderT computaton env  

     -- (<*>) ::                            f (a -> b) ->           f a ->           f b 
        (<*>) :: Applicative m => ReaderT l m (a -> b) -> ReaderT l m a -> ReaderT l m b
        (<*>) (ReaderT fab) (ReaderT fb) =
         let fab' = (<*>) <$> fab
             fb'  = fab'  <*> fb
         in ReaderT fb'
    -- ghci> import Data.Functor.Identity

    -- ghci> let plusTen =  pure (\x -> x + 10) :: ReaderT String Identity (Int -> Int) 
    -- ghci> let val = pure 42 :: ReaderT String Identity Int  
    -- ghci> let env = "My Environment" 
    -- ghci> runReaderT (plusTen <*> val) env 

-- ================================ Monad ======================================== -- 

instance (Monad m) => Monad (ReaderT l m)
    where
        return :: Monad m => r -> ReaderT l m r
        return = pure 
        
        (>>=) :: Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
        (>>=) (ReaderT rma) ab =  
             -- remember that the type of ReaderT itself is a function, that takes an "r"
                ReaderT $ \r ->
                    rma r >>= \a ->
                    let readerT = ab a                    
                    in runReaderT readerT r 

-- ================================ Example ======================================== -- 

-- a sample configuration type
data AppConfig = AppConfig { databaseHost :: String, databasePort :: Int}

-- computation using ReaderT
makeReaderT :: ReaderT AppConfig IO String
makeReaderT  =  
    ReaderT readerTFunc   
        where 
            readerTFunc :: AppConfig -> IO String
            readerTFunc config =
                let host = databaseHost config
                    port = databasePort config
                in  pure $ "Connected to database at " ++ host ++ ":" ++ show port


-- Run the computation with a specific configuration
runExample :: IO String
runExample = 
    runReaderT makeReaderT AppConfig { databaseHost = "localhost", databasePort = 5432 }
        