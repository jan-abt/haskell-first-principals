{-# LANGUAGE OverloadedStrings #-}
    
module Scotty where

import Web.Scotty ( ScottyM, ActionM, get, html, pathParam, scotty )
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class ( MonadTrans(lift) )

--   ======================================================================================== 
--   ================= Scotty Web's ActionT, using the "ReaderT" pattern ====================
--   ======================================================================================== 
--
--     ActionM is a type alias of the ActionT type for a specific underlying monad m (e.g., IO).
-- 
--         type ActionM = ActionT IO :: * -> *
-- 
--     The ActionT type provides an abstraction for building web actions in the context of the Scotty web framework.
-- 
--         newtype ActionT m a = ActionT {
--             runAM :: ReaderT ActionEnv m a
--         }
-- 
--     The composition of the following monads creates a stack of monad transformers
--     In this case, the monad transformer stack consists only of ReaderT ActionEnv.
-- 
-- 
--     ReaderT ActionEnv:
--     The ReaderT monad transformer adds a read-only environment of type ActionEnv.
--     It allows the action to access a shared environment, such as configuration or context information.
--     "action" refers to a computation or a piece of code that gets executed in response to a specific HTTP request. 
--     When a request matches a route defined in the web application, the associated action is triggered to handle that particular request.
--     The underlying monad is m.
   

--   ======================================================================================== 
--   =============================  The lift function ======================================= 
--   ======================================================================================== 

-- The lift function brings monadic actions into a monad transformer stack. 
-- It allows for the execution of monadic actions within the context of the another monad.
-- This is a powerful tool when we have a monad transformer stack and want to perform actions 
-- from a more basic monad within that stack.
--
-- lift :: (Monad m, MonadTrans t) => m a -> t m a 

--   ======================================================================================== 
--   ============================= Side Effect Recap ======================================= 
--   ======================================================================================== 

-- In Haskell, performing actions with side effects, like printing to the screen,
-- is done inside monadic structures. The `IO` monad is commonly used
-- to encapsulate these effects.

-- The first attempt below is incorrect because it attempts to concatenate
-- a pure `String` with an action that produces a side effect (IO action).

-- Incorrect Attempt:
{-
   sample :: String
   sample = 
       putStrLn "hello"   -- Produces a side effect of type IO ()
       "my String"        -- Returns a pure String, causing a type mismatch
-}
-- To perform both the side effect and obtain a result, we need to use monadic
-- constructs like `do` notation or the bind (>>=) operator. This ensures that
-- the sequencing of actions is done within the context of the appropriate monad.

-- SOLUTION 1: Using the bind operator (>>=)
-- The bind operator allows us to sequence IO actions and use their results.
{- 
   sample :: IO String
   sample = putStrLn "hello" >>= \_ -> return "my String"
-}

-- SOLUTION 2: Using the do notation
-- The do notation provides a more readable way to sequence IO actions.
{- 
    sample' :: IO String
    sample' = do
        _ <- putStrLn "hello"
        return "my String"
-}    
    
main :: IO ()
main = 
    scotty 3000 routeHandlers
    where 
     -- ScottyM is a type synonym for the main type representing a Scotty web application.
        routeHandlers :: ScottyM () 
        routeHandlers = get "/:word" actionGET

        {-
          "ActionM" is a type representing a monad transformer stack used for defining web actions in Scotty.
          When inside a "get", "post", or other route declarations, we're working in the "ActionM" monad.
        -}
        actionGET :: ActionM ()
        actionGET = do 
            beam <- pathParam "word"

            -- for clarification sake we are starting here with the
            -- original polimorphic type declaration of "lift",while then
            -- spcializing the types step by step down to our particular use case.
            -- 1 lift :: (Monad m, MonadTrans t) => m a -> t m a 
            -- 2 lift :: (MonadTrans t) => IO a -> t IO a
            -- Note: We go from (t IO a) to (ActionM a) because of the ActionM type alias 
            -- 3 lift :: IO a -> ActionM a
            -- 4 lift :: IO () -> ActionM ()
            
            _ <- (ActionT . lift ) (putStrLn "hello")
            html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]    
        
-- ======================================== Scotty Web's OLD ActionT  ========================================= --
-- since has been re-written to use the "ReaderT pattern" https://www.fpcomplete.com/blog/readert-design-pattern/ --
-- ============================================================================================================== --

  
--   ActionM is a type alias of the ActionT type for IO
--       
--       type ActionM = ActionT IO :: * -> *
--
--   The ActionT type provides a an abstraction for
--   building web actions in the context of the Scotty web framework.    
--
--       newtype ActionT e m a = ActionT {
--           runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a
--       } 
--
--   The composition of the following monads creates a stack of monad transformers 
--   that provides exception handling, a read-only environment, and mutable state.   
--
--       ExceptT (ActionError e): 
--   The ExceptT monad transformer allows for exception handling. 
--   It wraps the computation in the context of possible errors of type ActionError e.
--
--       ReaderT ActionEnv: 
--   The ReaderT monad transformer that adds a read-only environment of type ActionEnv. 
--   It allows the action to access a shared environment, such as configuration or context information.
--
--       StateT ScottyResponse m: 
--   The StateT monad transformer introduces state into the computation. 
--   It allows the action to maintain and modify the state, with ScottyResponse being the type of the state.
