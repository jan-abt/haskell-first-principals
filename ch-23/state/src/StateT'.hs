module
    StateT'
        where

import Control.Monad (join)

-- Constructor: StateT' (s -> m (a, s))	 
newtype StateT' s m a = StateT' {
    runStateT' :: s -> m (a, s)
}

-- State Functor--
instance (Functor m) => Functor (StateT' s m)
    where
        fmap :: (a -> b) ->  StateT' s m a -> StateT' s m b
        fmap f (StateT' sf ) = 
            StateT' (\s ->  (\(a,s') -> (f a, s')) <$> (sf s) )   
               
-- let stateFunctor =  (+5) <$> StateT' (\s -> Just (0,s))        
-- runStateT' stateFunctor "OK"

-- State Applicative --
instance (Functor m, Monad m) => Applicative (StateT' s m)
    where 
        pure :: a -> (StateT' s m a)
        pure a  = StateT' (\s -> pure (a, s))
        --  s -> m ((a->b), s)
        (<*>) :: StateT' s m (a -> b) -> (StateT' s m a) ->(StateT' s m b)
        (<*>) (StateT' f) (StateT' g) = 
            -- StateT' (\s -> (f s) >>=  (\(f', s') -> (\(a, s'') -> (f' a, s'') ) <$> (g s') ) )
            
            -- StateT' (\s -> (f s) >>=  
            --     (\(f', s') -> (g s') >>= 
            --         (\(a, s'') -> return (f' a, s'') ) ) 
            -- )

            StateT' (\s -> do 
                        (f', _) <- (f s)
                        (a, _)  <- (g s) 
                        return (f' a, s) 
                    )
            
    -- let fromPure :: StateT' String Maybe Int = pure 5
    -- runStateT' fromPure "OK"
    -- Just (5,"OK")

    -- let stateApplicative =  pure (+5) <*> StateT'(\s -> Just (10,s))  
    -- runStateT' stateApplicative "OK"
    -- Just (15,"OK")

-- State Monad --    
instance (Functor m, Monad m) => Monad (StateT' s m) 
    where
        return = pure
        (>>=) :: StateT' s m a -> (a -> StateT' s m b) -> StateT' s m b 
        (>>=) (StateT' m) f = 
                    -- StateT' (\s ->  
                        --     let r = fmap (\(a,_) -> 
                        --                 let (StateT' x) =  (g a)
                        --                 in x
                        --             ) (f s) 
                        --         t =  fmap (\x -> x s) r
                        --     in join t
                        -- )
                    StateT' (\s ->  
                        let mmb = fmap (\(a, s') -> 
                                    let StateT' smb  = (f a) 
                                    in smb s'
                             ) (m s) 
                        in join mmb
                    )
                         
-- let stateMonad = StateT' (\s -> Just (0, s))
-- let result = stateMonad >>= (\n -> StateT' (\s -> Just (n + 1, s)))
-- runStateT' result "OK"
-- Just (1,"OK")