module Lib (palindrome) where

import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO () 
palindrome = do
  raw <- getLine
  let cleaned = clean raw
  case (cleaned == reverse cleaned) of
    True -> do
      putStrLn "It's a palindrome!" 
      exitSuccess
    False -> do 
      putStrLn "Not a palindrome! Try again!"
      palindrome

clean :: String -> String
clean xs = [c | c <- map toLower xs,  c `elem` ['a'..'z'] ] 

{- 
  Sample palindromes

  A man, a plan, a canal, Panama!
  Racecar
  Able was I ere I saw Elba
  Never odd or even
  Step on no pets
  Madam, in Eden, I'm Adam
  A Toyota's a Toyota
  Was it a car or a cat I saw?
  A Santa at NASA
  Eva, can I see bees in a cave?

-}           
