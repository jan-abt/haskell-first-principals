module Main where

import qualified Arithmetic (sayHello)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Arithmetic.sayHello
