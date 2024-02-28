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
readerMaybeExample :: ReaderT Int Maybe String
readerMaybeExample = do
  env <- ask
  if env > 0
    then lift $ return "Positive"
    else lift $ return "Not Positive"

-- MaybeT (Reader r) a
maybeReaderExample :: MaybeT (Reader Int) String
maybeReaderExample = do
  env <- lift ask
  if env > 0
    then return "Positive"
    else return "Not Positive"


-- ============================== RUN EXAMPLES ==============================

-- Example function using ReaderT r Maybe
-- runReaderMaybeExample 0
-- runReaderMaybeExample 1
runReaderMaybeExample :: Int -> Maybe String
runReaderMaybeExample env = 
  runReaderT readerMaybeExample env

-- Example function using MaybeT (Reader r)
-- runMaybeReaderExample 0
-- runMaybeReaderExample 1
runMaybeReaderExample :: Int -> Maybe String
runMaybeReaderExample env = 
  runReader (runMaybeT maybeReaderExample) env
