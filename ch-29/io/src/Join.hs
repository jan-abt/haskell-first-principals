module Join where

import Control.Monad

embedInIO:: a -> IO a
embedInIO = pure

main ::IO ()
main = do
    v <- embedInIO (1 :: Int)
    print $ "The int value is " <> show v
    embedInIO (print "This has type IO (IO a) and thus is not going to be rendered!")

    -- merge the effects of both actions and get a single IO a which will print a result in GHCi
    join $ embedInIO (print "I'll put in some ingredients")
