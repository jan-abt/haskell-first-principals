module Main where

import Data.Char (toLower)
import Lib (randomWord', freshPuzzle, runGame)

main :: IO () 
main = do
 word <- randomWord'
 putStr "\nGuess a letter until you can make out the word! "
 putStrLn ("\n---- hint: " ++ word ++ " ----\n")
 let puzzle = freshPuzzle (fmap toLower word) 
 runGame puzzle


-- flatten :: IO WordList -> IO [String]
-- flatten nestedWordsList = do
--   wordList <- nestedWordsList >>= (\nestedWords ->  return $ map (" " ++) nestedWords)
--   return wordList
