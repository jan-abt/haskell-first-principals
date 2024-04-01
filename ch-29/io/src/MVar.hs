module MVar where

import Control.Concurrent
import Control.Exception (BlockedIndefinitelyOnMVar(..))
import Control.Exception.Base (catch)

allocateMVar :: IO (MVar Int)
allocateMVar = newEmptyMVar

succeed :: IO ()
succeed = do
    mv <- allocateMVar
    putStrLn "putMVar mv 0"
    putMVar mv 0
    zero <- takeMVar mv
    putStr "takeMVar mv "
    print zero
    putStrLn "putMVar mv 1 "
    putMVar mv 1
    one <- takeMVar mv
    putStr "takeMVar mv "
    print one


empty :: IO ()
empty = do
    mv <- allocateMVar
    catch
        (do 
            putStrLn "takeMVar mv"
            takeMVar mv
        ) -- take on empty, will block !!
        (\(err::BlockedIndefinitelyOnMVar) -> do
            putStrLn ("Caught: " <> show err)
            putStrLn "Reason: couldn't take from mVar because it was empty. Inserting 1, then retrieving."
            putMVar mv 1
            pure 1
        )    
    one <- takeMVar mv
    putStrLn $ "= " <> show one 


full :: IO ()
full = do
    mv <- allocateMVar
    putStrLn "putMVar mv 0"
    putMVar mv 0
    catch
        (do 
            putStrLn "putMVar mv 1"
            putMVar mv 1 
        ) -- put on full mVar, will block !!!
        (\(err::BlockedIndefinitelyOnMVar) -> do
            putStrLn ("Caught: " <> show err)
            putStrLn "Reason: couldn't put into mVar because it was full."
            putStrLn "Clearing previous value from mVar, then inserting 1."
            _ <- takeMVar mv
            putMVar mv 1
        )
    one <- takeMVar mv
    putStrLn $ "= " <> show one 



main :: IO ()
main = do

  succeed
  empty -- *** Exception: thread blocked indefinitely in an MVar operation     
  full -- *** Exception: thread blocked indefinitely in an MVar operation
  