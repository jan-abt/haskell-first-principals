{-# LANGUAGE InstanceSigs #-}
    
module
    Reader' (Reader'(..))
        where 


{-
The newtype ``Reader r a` `represents a computation that depends on a shared read-only environment
You can think of it as :
    * a computation that reads from an environment of type r  
    * and produces a result of type a
    * `runReader` extracts the underlying function from the Reader newtype.-}    
newtype Reader' r a =
    Reader' { runReader' :: r -> a }    

instance 
    Functor (Reader' r) 
        where
            fmap :: (a -> b) -> Reader' r a -> Reader' r b 
            fmap     f         (Reader' ra) =  Reader' (\r ->  f (ra r))    
          -- OR --  
          --fmap     f         (Reader' ra) =  Reader' (f . ra)    

instance Applicative (Reader' r) 
    where 
        -- the pure function allows you to lift a pure value into the Reader monad, 
        -- creating a computation that doesn't depend on the environment. 
        -- This can be useful when you want to embed constant values or computations 
        -- that don't need the environment in a Reader context.
        {- EXAMPLE
                --here we have a computation using pure
                pureComp :: Reader Int String
                pureComp = pure "Hi"

                main :: IO ()
                main = do
                -- now we run the computation with an environment:
                let myEnv = 42
                let result = runReader pureComp myEnv
                -- the result will always return a constant value of "Hi"
                putStrLn result  -}
        pure :: a -> Reader' r a
        pure a = Reader' $ (\_ -> a)

        (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
        (<*>) (Reader' rab) (Reader' ra) = Reader' (
         -- -- -- -- -- -- -- -- -- OPTION ONE -- -- -- -- -- -- -- -- -- --  -- 
         --              \r -> let fApplied = (fmap (\f -> f (ra r) ) rab )   --
         --                    in  fApplied r                                 --
         -- -- -- -- -- -- -- -- -- -- OR -- -- -- -- -- -- -- -- -- -- -- -- -- 
                         \r -> let fComposed = (fmap ( . ra ) rab ) r                
                               in  fComposed r
         )
        {- EXAMPLE            
            let r = pure (\n -> n +1 ):: Reader' String (Int -> Int)
            let f = runReader' r ""
                f 5

            let r =  Reader' (\n -> case n of 0 -> "N/A" ; _ -> "Yes") :: Reader' Int String
            let f = runReader' r
            f 1 -}                


instance Monad (Reader' r) 
    where 
        return = pure
        (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b 
      --SAME: (Reader' ra) aRb = Reader' (\r -> runReader' (aRb (ra r)) r)
        (>>=) (Reader' ra) aRb = Reader' (\r -> ( runReader' ( aRb . ra $ r ) ) r )

              -- runReader' ( aRb . ra )
     
                                 

