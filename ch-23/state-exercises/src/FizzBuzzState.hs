module
    FizzBuzzState
        where

import Control.Monad.Trans.State

-- Constructor: StateT: (s -> m (a, s))	 
-- StateT Record
{-
    newtype StateT s m a = StateT {
        runStateT :: s -> m (a, s)
    }
-}


fizzbuzzList :: [Integer] -> [[String]] 
fizzbuzzList ns =
    --  Map each element of `is` to a monadic action, evaluate 
    -- these actions from left to right, and ignore the results.
    let sc::  StateT [String] []  ()  = mapM_ evaluateEach ns
    -- Evaluate a state computation with the given initial state, []
    -- and return the final state, discarding the final value.
    in execStateT sc [] 

evaluateEach :: Integer -> StateT [String] [] ()
evaluateEach n = do
    xs <- get
    let result = fb n 
    put (result : xs)

fb :: Integer -> String
fb n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`5 == 0 ="Fizz" 
           | n `mod`3 == 0 ="Buzz" 
           | otherwise = show n

fbFromTo :: Integer -> Integer -> [String] 
fbFromTo a1 a2  = 
        if a1 < a2 
            then ft (\n -> n -1) [] a2 a1
            else ft (\n -> n +1) [] a2 a1
        where
            ft o xs v k | v /= k = ft o ((fb v):xs) (o v) k     
                        | otherwise = ((fb v):xs)
                             
main :: IO () 
main =
    mapM_ putStrLn $ fbFromTo 1 100