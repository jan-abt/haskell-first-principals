{-# LANGUAGE FlexibleContexts #-}

module EquivalenceTest where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe

{-
    ReaderT r Maybe and MaybeT (Reader r) are not exactly equivalent,
    but closely related and can often be used interchangeably depending on the context. 
    These types involve combining the Reader and Maybe monads in different orders.

    ReaderT r Maybe a: 
    * Represents a computation that has access to a shared environment r. 
    * May return a Maybe a result.

    MaybeT (Reader r) a: 
    * Represents a computation in the environment r that may fail.
    * May return a Maybe (Reader r a) result.  
-}

-- ReaderT r Maybe a
readerTMaybeExample :: ReaderT Int Maybe String
readerTMaybeExample = do
  env <- ask
  if env > 0
    then lift $ return "Positive"
    else lift $ return "Not Positive"

-- MaybeT (Reader r) a
maybeTReaderExample :: MaybeT (Reader Int) String
maybeTReaderExample = do
  env <- lift ask
  if env > 0
    then return "Positive"
    else return "Not Positive"

-- ============================== RUN EXAMPLES ==============================

-- Example function using ReaderT r Maybe
-- runReaderTMaybeExample 0
-- runReaderTMaybeExample 1
runReaderTMaybeExample :: Int -> Maybe String
runReaderTMaybeExample env = 
  runReaderT readerTMaybeExample env

-- Example function using MaybeT (Reader r)
-- runMaybeTReaderExample 0
-- runMaybeTReaderExample 1
runMaybeTReaderExample :: Int -> Maybe String
runMaybeTReaderExample env = 
  runReader (runMaybeT maybeTReaderExample) env
