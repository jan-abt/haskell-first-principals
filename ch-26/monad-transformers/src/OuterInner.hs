module OuterInner where


-- =================================== Data Types ======================================== --

-- The types listetd below are approximations of the types specified in "Control.Monad.Trans" 
-- Note that the actual definitions may look slightly different.

newtype MaybeT m a =
     -- MaybeT's type is: some Monad "m", that wraps a Maybe containing a value of type "a"
        MaybeT { runMaybeT :: m (Maybe a) }  

newtype ExceptT e m a =
     -- ExceptT's type is: some Monad "m", that wraps an Either containing a value of type "e" or "a"
        ExceptT { runExceptT :: m (Either e a) } 

newtype ReaderT r m a =
     -- ReaderT's type is: a computation reveiving some arg "a" and returning some Monad "m", that wraps a value of type "a"
        ReaderT { runReaderT :: r -> m a } 
        
-- =================================== "mk" functions ======================================== --

                  --   r  ->  m             a
getValueFromReaderT :: r  ->  IO (Either String (Maybe Int))
getValueFromReaderT = runReaderT mkReaderT 
 -- inner(mkReaderT) -- middle(mkExceptT) -- outer(mkMaybeT)
    where        --  ReaderT  r   m              a 
        mkReaderT :: ReaderT  r  IO (Either String (Maybe Int)) 
                 -- runExceptT :: ExceptT e m a -> m (Either e a) 
        mkReaderT = runExceptT mkExceptT
        
                  -- ExceptT  e            m            a
        mkExceptT :: ExceptT String (ReaderT r IO) (Maybe Int)  
                 -- runMaybeT :: MaybeT m a -> m (Maybe a) 
        mkExceptT = runMaybeT mkMaybeT

                 -- MaybeT                m                                    a
        mkMaybeT :: MaybeT (ExceptT String (ReaderT   r      IO))             Int
    --  mkMaybeT =  MaybeT (ExceptT        (ReaderT $ \_ ->  pure (Right (Just 1 ))))
    --  or, stepwise ...   
        mkMaybeT =  putValueIntoReaderT

                    -- MaybeT :: some Monad "m", that wraps a Maybe containing a value of type "a"
                    -- MaybeT                m                  a
putValueIntoReaderT :: MaybeT (ExceptT String (ReaderT r IO)) Int 
putValueIntoReaderT =  mkMaybeT
 -- outer(mkMaybeT) -- middle(mkExceptT) -- inner(mkReaderT)
    where        -- MaybeT               m                  a
        mkMaybeT :: MaybeT (ExceptT String (ReaderT r IO)) Int
        mkMaybeT  = MaybeT mkExceptT 

                  -- ExceptT :: some Monad "m", that wraps an Either containing a value of type "e" or "a" 
                  -- ExceptT    e         m             a
        mkExceptT :: ExceptT String (ReaderT r IO) (Maybe Int)
        mkExceptT =  ExceptT mkReaderT
       
                  -- ReaderT :: some computation expecting some arg of type "r" and returning some Monad "m", that wraps some value of type "a"
                  -- ReaderT r m           a
        mkReaderT :: ReaderT r IO (Either String (Maybe Int)) 
        mkReaderT =  ReaderT readerTComp 
            
        readerTComp :: r -> IO   (Either String (Maybe Int))
        readerTComp  = \_-> pure (Right         (Just 5 ))

        

main :: IO (Either String (Maybe Int))
main = 

    -- Recall that the type of ReaderT is a computation, 
    -- that expects a polymorphically typeed input argument "r", here the actual value is "()",
    -- and returns a Monad "m", which wraps some value of type "a".     
    -- "()" here simply means: no envitonment !
    -- This ReaderT doesn't actually use any specific data from its environment.
    -- you can see that "r" is ignored in readerTComp: \_ -> ...
    getValueFromReaderT ()
