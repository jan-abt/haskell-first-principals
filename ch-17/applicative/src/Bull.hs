module 
    Bull
        where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
 
data Bull =
    Fools
    | Twoo
    deriving (Ord, Eq, Show)

instance Semigroup Bull
    where 
    (<>) Fools someBull = someBull
    (<>) Twoo _   = Twoo
 
instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

instance Arbitrary Bull 
    where 
        arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance EqProp Bull 
    where (=-=) = eq 

main :: IO ()
main = quickBatch (monoid Twoo)