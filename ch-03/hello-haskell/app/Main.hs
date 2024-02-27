module Main where

import HaskellSay (haskellSay)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  haskellSay "Hello from the \"haskellSay\" package!"
