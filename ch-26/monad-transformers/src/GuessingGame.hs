module GuessingGame where

import System.Random ( randomRIO )
import Data.Bool ( bool )
import qualified Data.Map as M
import Control.Monad.State
import Debug.Trace (traceIO)
import System.Exit ( exitSuccess )  

import Control.Monad (forever)
import System.IO 

{-
   Example of how a StateT Monad transformer could be used.

   A Tally of numbers, entered by user input and by the computer, via random generation
   is stored in a map, keyed by "<user>" and "Computer".
   After the user enters his 5th and final number, the numbers stored in the "GuessingGameMonad"
   are summed up and the result, evaluated as being either even or odd, is then printed to the screen.
   
   Example Result:
  [("Computer",16,"The sum of guesses is an even number"),("Tom",9,"The sum of guesses is an odd number")]
-}    

data Tally = Tally {
    guessesByPlayer :: M.Map String [Int],
    roundsSoFar :: Int
} deriving (Show)

type GuessingGameMonad a = StateT Tally IO a
type Results = [(String, String)]

main :: IO ()
main = do
    let initialState = Tally M.empty 1
    putStrLn "Enter your name! "
    _ <- hSetEcho stdin False  -- Turn off echoing in the terminal    
    whoami <- getLine
    putStrLn ("Hello " ++whoami++ ".\nThis game has 5 rounds.\nEach time, please enter a number between 0 and 5!")
    _ <- runStateT (playGame whoami) initialState 
    pure ()

playGame :: String ->GuessingGameMonad ()
playGame playerName = 
 forever $ do
    sb <- get
    if roundsSoFar sb <= 5
    then 
       continueGame playerName
    else
       exitGame

continueGame :: String ->GuessingGameMonad ()
continueGame playerName = do    
    value      <- lift $ read <$> getLine
    scoreBoard <- get
    update1    <- liftIO $ updateScoreBoard playerName value scoreBoard
    _          <- put update1
    rand       <- liftIO $ randomInt 0 5
    update2    <- liftIO $ updateScoreBoard "Computer" rand  update1
    put $ Tally (guessesByPlayer update2) (1 + roundsSoFar update2)   
    where        
        randomInt :: Int -> Int -> IO Int
        randomInt mn mx = randomRIO (mn, mx) 

updateScoreBoard :: String -> Int -> Tally -> IO Tally
updateScoreBoard k v sb = do
    let m = guessesByPlayer sb
    let rs = roundsSoFar sb
    case M.lookup k m of
        Nothing -> do
            let inserted = M.insert k [v] m
            traceIO $ "inserted ("++(show rs ++"/5)")++": "  ++ show (M.toList inserted)
            pure $ Tally inserted rs
        Just xs -> do
            let val = xs ++ [v]
            let updated = M.insert k val m
            traceIO $ "updated ("++(show rs ++"/5)")++": "  ++ show (M.toList updated)
            pure $ Tally updated rs


exitGame:: StateT Tally IO ()
exitGame = do
    sb <- get
    _ <- liftIO $ traceIO (show $ displayScores sb)
    liftIO exitSuccess

displayScores :: Tally -> Results
displayScores sb = 
    let ks = M.keys $ guessesByPlayer sb
    in (\k -> 
        let v = case M.lookup k (guessesByPlayer sb) of
                (Just value) -> 
                    let summed = sum value
                        isEven = even (sum value)
                        x = ("The sum of guesses, "++ show summed )
                        evenOrOddSum = bool (x ++ " is an odd number") (x ++ " is an even number") isEven                      
                    in (k, evenOrOddSum)
                _      -> (k, "-")
        in v
     ) <$> ks


