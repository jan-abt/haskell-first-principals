module Main where

import Data.Bool
import Lib

type Password = String
type Data = String
type Attempts = Int
type Guessed = Bool

data Cipher = 
  Cipher Password Data Attempts Guessed

instance Show Cipher 
 where 
  show (Cipher pwd _ _ False ) =  "Your input was password encoded with the pasword you provided: " ++ map (\_-> '*' ) pwd
  show (Cipher _ decrypted _ True ) =  "The decoded cipher reads as: " ++ decrypted

main :: IO ()
main = do
  putStrLn "Enter a line of text you want to encode: "
  text <- getLine
  putStrLn "Enter a password, with which you can decode it: "
  pwd <- getLine
  let encodedCipher =  encodeInput pwd text
  putStrLn $ show encodedCipher 
  decodeInput encodedCipher

encodeInput :: String -> String -> Cipher
encodeInput pwd dat =  Cipher pwd (encode pwd dat) 0 False

decodeInput :: Cipher -> IO () 
decodeInput  cip@(Cipher _ _ attempts _ ) = do 
  done <- try cip
  if(not done) 
  then do
    let retryMessage = ("Try again with another password ("++(show attempts)++"/"++(show maxAttempts)++")")
        initialMessage = ("Enter the password, you have "++(show maxAttempts)++" chances to get it right") 
    putStrLn $ bool retryMessage  initialMessage (attempts == 0)
    pwd <- getLine
    verifyPassword cip pwd >>= decodeInput
  else 
    return ()  

verifyPassword :: Cipher -> String -> IO Cipher
verifyPassword (Cipher pwd xs n _) guess = do
 putStrLn $ "---------------------------------" 
 case (guess == pwd) of 
  True -> do
     return (Cipher pwd (decode pwd xs) n True)
  _ -> do
     putStrLn $ "Sorry, '"++guess++"' is not quite it, try again."
     return  (Cipher pwd xs  (n+1) False)

try :: Cipher -> IO (Bool)
try cip@(Cipher _ _ attempts success) =
  case success of
    False -> 
      if attempts > 3
      then do
        putStrLn "Too many failed attempts!"
        return (True) 
      else do
        return (False)
    _   -> do
     putStrLn $ show cip 
     return (True)     

maxAttempts :: Int
maxAttempts = 3     


