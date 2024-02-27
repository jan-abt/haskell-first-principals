module Jammin where

import Data.List

data Fruit =
   Apple
 | Blackberry
 | Peach
 | Plum
 | Rasberry
 deriving (Eq, Show, Ord)

data JamJars = 
 Jam {
     flavor :: Fruit,
     jars :: Int
  }
 deriving (Eq, Show, Ord)

row1 = Jam Plum  12 
row2 = Jam Peach 12 
row3 = Jam Apple 25 
row4 = Jam Plum  4
row5 = Jam Rasberry 34
row6 = Jam Blackberry 45 
row7 = Jam Peach 2 
row8 = Jam Apple 39


allJam = [row1, row2, row3, row4, row5, row6, row7, row8]


amounts = map jars allJam 

amountsSummed = sum amounts

mostRow = foldr (\e a -> if (jars e > jars a) then e else a ) row1 allJam

sortJam = 
  sortBy (  \e e' -> flavor e `compare` flavor e' ) allJam

groupJam = 
  groupBy ( \e e' -> flavor e == flavor e' ) sortJam
