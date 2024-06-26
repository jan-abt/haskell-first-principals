module NestedIO where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
    t <- getCurrentTime
    let (_, _, dayOfMonth) = toGregorian (utctDay t)
    if even dayOfMonth 
     then 
        return $ Left randomIO 
     else 
        return $ Right (putStrLn "no soup for you")


main :: IO ()
main = do 
    eith <- huehue
    case eith of 
        Left l -> l >>= print
        Right r -> r >>= print 
 