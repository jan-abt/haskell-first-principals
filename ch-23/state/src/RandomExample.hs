module 
	RandomExample 
		where

import System.Random
import Control.Monad.Trans.State
import Control.Applicative (liftA3) 
import Control.Monad (replicateM)
import Data.Foldable (sum)

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

-- Six-sided die
data Die =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Show)  

intToDie :: Int -> Die 
intToDie n =
  case n of
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
      --  this tactic _extremely_ sparingly.
    n' -> error $ "intToDie got non 1-6 integer: " ++ show n'


rollDieThreeTimes :: (Die, Die, Die) 
rollDieThreeTimes = do
  -- produces the same results every 
  -- time because it is free of effects.
  -- This is fine for this demonstration.
  let g = mkStdGen 0
      (r1, g1) = randomR (1, 6) g
      (r2, g2) = randomR (1, 6) g1 
      (r3, _)  = randomR (1, 6) g2
  (intToDie r1, intToDie r2, intToDie r3)
 

rollDie :: ( Monad m)=> StateT StdGen m Die 
rollDie =  generateState''    
-- runStateT rollDie $ mkStdGen 5

generateState ::(RandomGen s, Monad m)=> StateT s m Die
generateState = 
     let sv = randomR (1, 6)  
     in state $ (\tpl -> (intToDie $ fst tpl, snd tpl)) <$> sv

generateState' ::(RandomGen s, Monad m)=> StateT s m Die
generateState' = 
  -- state :: Monad m => (s      -> (v,    s)     ) -> StateT s      m        v 
  -- state ::            (StdGen -> (Die,  StdGen)) -> StateT StdGen Identity Die
     state $ do
       (n, g) <- randomR (1, 6)  
     -- g -> (Die, g)
       return (intToDie n, g)


generateState'' ::(RandomGen s, Monad m)=> StateT s m Die
generateState'' = 
      let sv = randomR (1, 6)  
          stateT = (state sv)  
      --  stateT:: StateT s m Int 
      -- fmap is possible, see functor instance info 
      -- :i StateT
      -- instance [safe] Functor m => Functor (StateT s m)
      -- fmap will map over a value that matches the last element type of StateT's type declaration, the Int.
      -- 
      in fmap intToDie stateT       


nDie :: (Monad m) =>Int -> StateT StdGen m [Die]
nDie n = replicateM n rollDie     
--  evalStateT (nDie 5) (mkStdGen 0)

 
rollsToGetTwenty :: StdGen -> (Int, [Die]) 
rollsToGetTwenty g =  
  let rolls = rollsToGetN [] g (limit 20)
  in (length rolls, intToDie <$> rolls)
 

limit:: (Ord a, Num a) => a -> a -> Bool
limit l r = l <= r

rollsToGetN :: [Int] -> StdGen -> (Int -> Bool) -> [Int]
rollsToGetN rs gen limitReached = 
  let (r, nextGen) = randomR (1, 6) gen 
      rolls     = r:rs
  in case (limitReached $ sum rolls) of
      True -> rolls
      _    -> rollsToGetN (rolls) nextGen limitReached
      
       
       
       
-- rollsToGetTwenty (mkStdGen 0)        
{-
  We can also use randomIO, which 
  uses IO to get a new value each time 
  without needing to create a unique value for the StdGen.  
-}  
--  (rollsToGetTwenty . mkStdGen) <$> randomIO 