module Main where

import qualified Lib (spitItOut)


main :: IO ()
main = Lib.spitItOut "The function used to print this line was specified as part of a dependent project's library."
