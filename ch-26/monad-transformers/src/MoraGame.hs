module MoraGame where

import System.Random (randomRIO)
import Control.Monad.Trans.State ( StateT, runStateT, modify, get )
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Debug.Trace (traceIO)
import System.Exit ( exitSuccess )
import Control.Monad (forever)

-- Define the Mora data type
data Guess = Rock | Paper | Scissors deriving (Show, Eq, Read)
data Winner = Player | Computer | Tie  deriving (Show, Eq, Read)

-- a type alias defining the StateT transformer monad for our purposes
--  e.g., a specialization of StateT
--  MoraGameMonad a = StateT GameState IO a
--  MoraGameMonad () = StateT GameState IO a 
-- () because we don't return a result. We're going to interact with the state 
type MoraGameMonad a = StateT GameState IO a 

-- Define the game state
data GameState = GameState
  { playerGuesses :: [Guess]
  , computerGuesses :: [Guess]
  , scores :: Scores
  , roundsPlayed :: Int
  } deriving (Show)

data Scores = Scores {
  player :: Int,
  computer :: Int,
  tie :: Int
} deriving (Show)

playGame ::  MoraGameMonad ()
playGame =
 forever $ do
    gs <- get
    if roundsPlayed gs < 5
    then
       playMoraRound
    else
       exitGame

exitGame::  MoraGameMonad ()
exitGame = do
    gs <- get    
    lift $ traceIO $ show $ scores gs
    liftIO exitSuccess

-- Play one round of Mora in MoraGameMonad
playMoraRound :: MoraGameMonad () 
playMoraRound = do
  -- Lift the result of the players's input action into MoraGameMonad context
  pGuess <- lift $ read <$> getLine
  cGuess <- lift generateGuess

  -- Update the state
  modify (\gameState ->
    
    let pGuesses  = pGuess : playerGuesses gameState
        cGuesses  = cGuess : computerGuesses gameState
        rplayed = 1 + roundsPlayed gameState
        
    in gameState {
        playerGuesses=pGuesses,
        computerGuesses=cGuesses,
        roundsPlayed=rplayed } )

  determineRoundWinner

-- Generate a random int between 0 and 2, convert that to "Guess"
generateGuess :: IO Guess
generateGuess = do
  randomIndex <- randomRIO (0, 2) :: IO Int
  return $ case randomIndex of
    0 -> Rock
    1 -> Paper
    _ -> Scissors



determineRoundWinner :: MoraGameMonad () -- 
determineRoundWinner = do

  -- Extract guesses from the game's state
  gameState <- get
  let (pGuesses, cGuesses, rp) = 
        (playerGuesses gameState, computerGuesses gameState, roundsPlayed gameState)

  -- Lift IO actions into MoraGameMonad context and print the current state to the terminal
  liftIO $ traceIO $ "("++show rp++") you chose: " ++ show (head pGuesses) ++ ", computer chose: " ++ show (head cGuesses) ++ "!"

  -- determine Winner
  let winner = case (head pGuesses,  head cGuesses) of
            (a, b) | a == b -> Tie
            (Rock, Scissors) -> Player
            (Scissors, Paper) -> Player
            (Paper, Rock) -> Player
            _ -> Computer

  -- update state of Scores
  modify (\gs -> 
      let (Scores player computer tie) = scores gs      
      in gs {
          scores = case winner of
            Player ->  Scores (player+1) computer tie
            Computer ->  Scores player (computer+1) tie
            _ -> Scores player computer (tie+1)
      }
   ) 

main :: IO ()
main = do
  putStrLn "Welcome to Mora!"
  putStrLn "Choose your action: Rock, Paper, or Scissors"

  -- Run our specialized MoraGameMonad version of a StateT transformer
  let initialState = GameState [] [] (Scores 0 0 0)0
  _ <- runStateT playGame initialState
  pure ()
