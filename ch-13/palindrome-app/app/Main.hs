module Main where

import qualified Lib (palindrome)

main :: IO ()
main = do
  putStrLn "Enter Palindrome"
  Lib.palindrome
