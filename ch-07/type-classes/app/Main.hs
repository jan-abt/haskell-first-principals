module 
	Main 
		where

import qualified Lib (sayHi)

main :: IO ()
main = do
  putStrLn "Hello Haskell!"
  Lib.sayHi
