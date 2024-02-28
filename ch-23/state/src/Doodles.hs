module 
	Doodles 
		where

import System.Random
import Data.Monoid

doodles :: IO () 
doodles = do
  let sg = mkStdGen 0
  print $ fst $ next sg
  print $ fst $ randomR (0::Int, 1000) sg
  let sg2 = snd (next sg)
  print $ fst $ next sg2
  print $ fst $ randomR (0::Int, 1000) sg2
  let sg3 =  snd (next sg2)
  print $ fst $ next sg3
  print $ fst $ randomR (0::Int, 1000) sg3
  let sg4 =  snd (next sg3)
  print $ fst $ next sg4
  print $ fst $ randomR (0::Int, 1000) sg4    


{-
 Implementations of newtype must have the same underlying representation as the type they wrap, 
 as the newtype wrapper disappears at compile time. 
 So the function contained in the newtype must be isomorphic to the type it wraps
-}    
type Iso a b = (a -> b, b -> a)    
--                             :: Iso (a  ->     b,     b -> a)
--                             :: Iso (a  -> Sum a, Sum a -> a)
--                             :: Iso  a  b
sumIsIsomorphicWithItsContents :: Iso  a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)
-- Sum :: a -> Sum a
-- getSum :: Sum a -> a

{-  
    StateT newtype
data constructor and runState record accessor as a means of 
putting a value in and taking a value of out the State' type.

State is a function that:
* takes input state s and 
* returns an output value 𝑎, tupled with the new state value. 


random :: (Random a, RandomGen g) => g ->   (a, g)
StateT {                runStateT :: s -> m (a, s)}

-}
newtype StateT' s m a = StateT' {
    runStateT' :: s -> m (a, s)
}

