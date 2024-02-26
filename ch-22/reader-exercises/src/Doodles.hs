module 
    Doodles 
        where

import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Char
 
plus10 :: (Num a) => a -> a
plus10 = (+10)

times2 :: (Num a) => a -> a
times2 = (*2)

composedTimes2Plus10 :: (Num a) => a -> a
composedTimes2Plus10 =  plus10  . times2

-- fmap works too
-- our Functor to fmap over, a partially applied function is also known as  the Functor of functions
-- the functorial context or structure to fmap over, is the partially applied function  "plus10"
fmappedTimes2Plus10 :: (Num a) => a -> a
fmappedTimes2Plus10 = fmap times2 plus10 

applicativeTimes2Plus10 :: Integer -> Integer 
applicativeTimes2Plus10 = (+) <$> times2 <*> plus10
-- ((+) <$> (*2) <*> (+10)) 3
--  (+)     (6)      (13) 

-- Mapping a function awaiting two arguments  
--  over a function awaiting one,  produces a two-argument function    
--    (fmap (+) (*2) )    5 3
--  produces a two-argument function    
--    (\a b -> (2 * a) + b )    5 3
--  same, expressed as function composition
--  ((+) . (*2)) 5 3
--  (\ x -> (+) (2 * x)) 5 3 
--  (\ 5 -> (+) (2 * 5)) 3 
--  ((+) 10) 3
--  13
a2ApplicativeTimes2Plus10 :: Integer -> Integer 
a2ApplicativeTimes2Plus10 = liftA2 (+) times2 plus10    

doTimes2Plus10:: Integer -> Integer 
doTimes2Plus10 = do
    a <- times2
    b <- plus10 
    return (a + b)    

----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------


cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs        

composedRevCap :: [Char] -> [Char]
composedRevCap = rev . cap 

bindComposedRevCap :: [Char] -> [Char]
bindComposedRevCap = pure rev >>= (\r -> (r . (pure cap >>= \c -> c )))  

fmappedRevCap :: [Char] -> [Char]
fmappedRevCap = fmap rev cap

applicativeTupledRevCap :: [Char] -> ([Char], [Char])
applicativeTupledRevCap = (,) <$> rev <*> cap   

doTupledRevCap :: [Char] -> ([Char], [Char])
doTupledRevCap = do
    r <- rev
    c <- cap
    return (r, c)

bindTupledRevCap :: [Char] -> ([Char], [Char])
bindTupledRevCap =  pure rev  >>= (\r -> (,) <$> r <*> cap)
   
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------    

-- Explanation of type Reader: 
-- The Reader type is essentially a type synonym for ReaderT, 
-- while specialized to the Identity monad.

-- (Reader Int Int) can be considered a monomorphic application of
-- ReaderT requiring an argument of type Int and returning a result type Int, 
-- wrapped in the Identity monad.
askReaderI:: Reader Int Int
askReaderI = (reader id) 
-- let f = runReaderT askReaderI
-- f 10

---------------------------------------------
 -- askReaderI is the same as askReaderI' --  
---------------------------------------------
    
askReaderI' :: ReaderT Int Identity Int
askReaderI' = (ReaderT Identity ) 
-- let f = runReaderT askReaderI'
-- f 10

askReaderT :: ReaderT Int Maybe String
askReaderT = ReaderT (\x -> 
                case x of 
                    1 -> Just "OK" 
                    _ ->  Nothing
             )
-- let f = runReaderT askReaderT 
-- f 1

----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------

-- Again, keep in mind that the Reader type is synonymous with type ReaderT, 
-- while specialized to the Identity monad.    
askz :: (r -> a) -> Reader r a 
askz f = reader f    
-- let f = runReaderT $ askz (\n -> n *2 )
-- f 10

-- for more flexibility, use ReaderT and provide your own Monad
askz' :: (Monad m ) => (r -> m a) -> ReaderT r m a 
askz' f = ReaderT f    
-- let f = runReaderT (askz' (\n -> Right $ n *2 )) 
-- f 10

