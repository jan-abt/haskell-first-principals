{-# LANGUAGE FlexibleContexts #-}

module MonadTransformerStackExample where

import Control.Monad.Reader
import Control.Monad.State (MonadState(put, get))
import Control.Monad.Trans.State ( StateT, runStateT ) 
import Control.Monad.Trans.Writer ( WriterT, runWriterT, tell) 

{-
    Monad Transformer Stack
      * is a combination of multiple monad transformers applied in a specific order 
        to create a composite monad that captures the effects of all the nested transformers. 
      * used to combine different monadic effects into a single monad.
    
    Recap:  ReaderT monad transformer 
      * designed to carry an environment throughout the computation.
      * here, the "ask" function is used within the context of the 
        "ReaderT Int Maybe" monad transformer stack. 
      * here, ask is providing access to the 
        environment value of type "Int" within the "ReaderT" monad. 

    Recap: lift function      
      * used to lift a computation from an inner monad (here, Maybe) 
        to the combined monad (ReaderT Int Maybe). 
      * below, it's lifting the return "Positive" and return "Not Positive" computations 
        from the Maybe monad to the ReaderT Int Maybe monad transformer stack.
      
-}


type ReaderWriterMaybeTransformerStack a = ReaderT Int (WriterT String Maybe) a

readerWriterMaybeComputation :: ReaderWriterMaybeTransformerStack String
readerWriterMaybeComputation = do
  ask >>= \env ->
    lift (tell "Logged something here") >>
      if env >= 0
        then return $ show env ++ " is a positive value"
        else return $ show env ++ " is a negative value"

type ReaderStateMaybeTransformerStack a = ReaderT Int (StateT Int Maybe) a

readerStateMaybeComputation :: ReaderStateMaybeTransformerStack String
readerStateMaybeComputation =
  ask >>= \env ->
    get >>= \state ->
      if env >= 0
        then do
          put (state+1)
          return $ show env ++ " is a positive value"
        else do
          put (state+1)
          return $ show env ++ " is a negative value"


readerPrintAndInc :: (Num a, Show a) => ReaderT a IO a
readerPrintAndInc =
   ask >>= \env -> 
        lift $ putStrLn ("Hi: " ++ show env) >> 
            pure (env+1)


-- ============================== RUN EXAMPLES ==============================

runReaderWriterMaybeComputation :: Int -> Maybe (String, String)
runReaderWriterMaybeComputation env = 
  runWriterT $ runReaderT readerWriterMaybeComputation env  
-- runReaderWriterMaybeComputation 100
-- runReaderWriterMaybeComputation (-100) 

runReaderStateMaybeTransformerStack :: Int -> Int -> Maybe (String, Int)
runReaderStateMaybeTransformerStack env = 
  runStateT (runReaderT readerStateMaybeComputation env)
-- runReaderStateMaybeTransformerStack 100 0
-- runReaderStateMaybeTransformerStack (-100) 0

runReaderPrintAndInc :: Integer -> IO Integer
runReaderPrintAndInc = runReaderT readerPrintAndInc
-- runReaderPrintAndInc 1

runReaderPrintAndIncMap :: [Integer] -> IO [Integer]
runReaderPrintAndIncMap = mapM (runReaderT readerPrintAndInc)
-- runReaderPrintAndIncMap [1..10]
