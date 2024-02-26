module
    Exercises
        where

import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity

-- Constructor: StateT: (s -> m (a, s))	 
-- StateT Record
{-
    newtype StateT s m a = StateT {
        runStateT :: s -> m (a, s)
    }
-}

-- State where the state is also the value you return
get' :: (Monad m) => StateT s m s 
get' = StateT (\s -> return (s, s))

-- runStateT get' $ "wilma"
--   ("wilma","wilma")

-- Construct State where from the argument 
-- provided and default to a value of unit.
put' :: Monad m => s -> StateT s m ()
put' s = StateT (\_ -> return ((), s))

-- runStateT (put' "wilma") "daphne"
-- ((),"wilma")    

-- Evaluate a state computation with the given state s, and 
-- return the final state, discarding the final value.
execStateT' :: Monad m => StateT s m a -> s -> m s
execStateT' (StateT sma) s =  snd <$> (sma s) 

-- execStateT' (put' $ Just "wilma") $ Just "daphne"
-- "wilma"


-- Evaluate a state computation with the given state and return the final value, 
-- discarding the final state
evalStateT' :: Monad m => StateT s m a -> s -> m a
evalStateT' (StateT sma) s  = fst <$> (sma s)     

-- evalStateT' get' "bunnicula"
-- "bunnicula"  

-- a function which applies a function to create a new State.
modifyT' :: Monad m => (s -> s) -> StateT s m ()  
modifyT' sf = 
    let g =  get' 
    in  g >>= (put' . sf)

-- runStateT (modifyT' (+1)) 0
-- ((),2)
-- runStateT (modifyT' (+1) >> modifyT' (+1) >> modifyT' (+1)) 0 
-- ((),3)

--  execStateT' (modifyT' (+1) >> modifyT' (+1) >> modifyT' (+1)) 0     