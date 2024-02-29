{-# LANGUAGE FlexibleContexts #-}

module EquivalenceTest where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe

{-
    "ReaderT r Maybe a" and "MaybeT (Reader r) a" are not exactly equivalent.
    They are often be used interchangeably depending on the context. 

    In essence, the difference lies in the order of stacking the monad transformers: 
     * readerTMaybeExample
        a "Maybe String" monad is encapsulated within a "ReaderT Int" monad transformer.
     * maybeTReaderExample
          a "Reader Int" monad is encapsulated within a "MaybeT String" monad transformer, 


    "ReaderT r Maybe a": 
    * Represents a computation that has access to a shared environment r. 
    * May return a Maybe a result.

    "MaybeT (Reader r) a": 
    * Represents a computation in the environment r that may fail.
    * May return a Maybe (Reader r a) result.  

    Recap:  ReaderT monad transformer 
      * designed to carry an environment throughout the computation.
      * here, the "ask" function is used within the context of the 
        "ReaderT Int Maybe" monad transformer stack. 
      * here, ask is providing access to the 
        environment value of type "Int" within the "ReaderT" monad. 

-}

-- ReaderT r Maybe a
readerTMaybeExample :: ReaderT Int Maybe String
readerTMaybeExample =
  ask >>= \env ->
    if env > 0
      then lift $ return "Positive"
      else lift $ return "Not Positive"

-- MaybeT (Reader r) a
maybeTReaderExample :: MaybeT (Reader Int) String
maybeTReaderExample =
   ask >>= \env ->
    if env > 0
      then return "Positive"
      else return "Not Positive"

-- ============================== RUN EXAMPLES ==============================

-- function using ReaderT r Maybe
runReaderTMaybeExample :: Int -> Maybe String
runReaderTMaybeExample env = 
  runReaderT readerTMaybeExample env
-- runReaderTMaybeExample 0
-- runReaderTMaybeExample 1

-- function using MaybeT (Reader r)
runMaybeTReaderExample :: Int -> Maybe String
runMaybeTReaderExample env = 
  runReader (runMaybeT maybeTReaderExample) env
-- runMaybeTReaderExample 0
-- runMaybeTReaderExample 1
