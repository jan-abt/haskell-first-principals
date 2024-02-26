module Main where 

import qualified Lib (sayHello)
import System.IO

main :: IO () 
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine 
  Lib.sayHello name

-- do syntax, is syntactic sugar, 
-- used inside functions that return an IO action, here of unit,
-- in order to sequence side effects in a convenient syntax.
