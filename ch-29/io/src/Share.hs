module Share where

import Debug.Trace

-- trace function: 
-- outputs the trace message given as its first argument, 
-- before returning the second argument as its result.
blah :: IO String
blah = trace "outer trace" blah'

blah' :: IO String 
blah' = return "blah"

woot :: IO String
woot = pure $ trace "inner trace" "woot"

main :: IO ()
main = do
    b <- blah
    putStrLn b 
    putStrLn b 
    w <- woot 
    putStrLn w 
    putStrLn w

{- 
    prints: 
        outer trace
        blah
        blah
        inner trace
        woot
        woot        
-}    