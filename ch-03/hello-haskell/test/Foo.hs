module Main where

import Test.QuickCheck

main :: IO()
main = quickCheck $ verbose $ prop_reverse "reversed once and then again"
    
prop_reverse :: [Char] -> Property
prop_reverse xs = (reverse . reverse) xs === xs
