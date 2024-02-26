module Lib where

import Control.Monad (forever)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess) 
import System.Random (randomRIO)
import Test.QuickCheck

type WordList = [String]

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 5

allWords :: IO WordList
allWords = do
 dict <- readFile "data/dict.txt" 
 return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where 
   gameLength w =
    let l = length (w :: String)
    in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String 
randomWord wl = do
 gmw <- gameWords
 randomIndex <- randomRIO (0, (length gmw) - 1)
 return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

  -- ==================================================
                  -- the string to guess | matching chars filled in so far | missed guesses so far
data Puzzle = 
  Puzzle { 
    getWord :: String, 
    getMatchedChars :: [Maybe Char], 
    getGuessedChars :: String
  } 
  deriving (Eq)

instance Show Puzzle 
  where 
    show (Puzzle _ discovered guessed) = (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

renderPuzzleChar ::  Maybe Char -> Char
renderPuzzleChar (Just c)  = c
renderPuzzleChar Nothing  = '_'

  -- take the puzzle word and turn it into a list of Nothing
freshPuzzle :: String -> Puzzle 
freshPuzzle xs = Puzzle xs (map (\_ -> Nothing) xs) []

charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle xs _ _ ) c = c `elem` xs

alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle _ _ xs ) c = c `elem` xs

--  will need to be called repeatedly, using the output of the previous call as input to the next one. 
--
--   _ _ _ _ "word" [Nothing, Nothing,  Nothing,  Nothing] ""
--   _ _ r _ "word" [Nothing, Nothing,  Just 'r', Nothing] ""
--   _ o r _ "word" [Nothing, Just 'o', Just 'r', Nothing] ""
--
fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle theWord matchedChars guessedPreviously) newlyGuessedChar = 
 Puzzle theWord updateMatchedChars (newlyGuessedChar : guessedPreviously)
  where 
   updateMatchedChars = zipWith (matchNewChar newlyGuessedChar) theWord matchedChars
   matchNewChar currentGuess wordChar matchedChar = 
    if 
     currentGuess == wordChar
    then 
     Just currentGuess 
    else 
     matchedChar
  
handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
 putStrLn $ "Your guess was: " ++ [guess]
 case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
  (_, True) -> do
     putStrLn "You already guessed that character, pick something else!"
     return puzzle 
  (True, _) -> do
     putStrLn "This character was in the word, filling in the word accordingly"
     return (fillInCharacter puzzle guess)
  (False, _) -> do
     putStrLn "This character wasn't in the word, try again."
     return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
 if 
  (length guessed) > 7 
 then do 
  putStrLn "You lose!"
  putStrLn $ "The word was: " ++ wordToGuess
  exitSuccess 
 else 
  return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
 if 
  all isJust filledInSoFar 
 then do 
  putStrLn "You win!"
  exitSuccess 
 else 
  return ()

runGame :: Puzzle -> IO () 
runGame puzzle = 
 forever $ do
   gameOver puzzle
   gameWin puzzle
   putStrLn $ "The current state of the puzzle is: " ++ show puzzle 
   guess <- getLine
   case guess of
    c:[] -> handleGuess puzzle c >>= runGame 
    _   -> putStrLn "Your guess must be a single character"

