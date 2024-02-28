module
    KleisliComposition
        where

import Control.Monad ((>=>))
{-

  Left-to-right composition of Kleisli arrows.
    (bs >=> cs) a
     can be understood as the do expression
     do 
      b <- bs a 
      cs b
  
  -}

greet :: String -> IO String 
greet g = do
  putStrLn g
  getLine

readM :: Read a => String -> IO a 
readM = return . read

getAge :: String -> IO Int 
--    Left-to-right composition 
getAge g = greet >=> readM $ g

askForAge :: IO Int
askForAge = getAge "Greetings! How old are you? "