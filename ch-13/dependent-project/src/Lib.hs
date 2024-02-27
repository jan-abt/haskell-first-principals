module Lib where

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")

spitItOut :: String -> IO ()
spitItOut xs = putStrLn (xs)
