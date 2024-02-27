module 
      WordNumber (digitToWord, digits, wordNumber)
            where

import Data.List (intersperse)

digitToWord :: Int -> String 
digitToWord 0  = "zero"
digitToWord 1  = "one"
digitToWord 2  = "two"
digitToWord 3  = "three"
digitToWord 4  = "four"
digitToWord 5  = "five"
digitToWord 6  = "six"
digitToWord 7  = "seven"
digitToWord 8  = "eight"
digitToWord 9  = "nine"

digits :: Int -> [Int] 
digits n = step (show n) [] 
   where step [] ns =  ns
         step (c:cs) ns = step cs (ns++[read[c]])

wordNumber :: Int -> String
wordNumber digs = concat $ intersperse "-" $ map digitToWord $ digits digs

