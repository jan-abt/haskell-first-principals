module Print04 where

printSecond :: IO () 
printSecond = do
  putStrLn greeting
  
main :: IO () 
main = do
  putStrLn greeting 
  printSecond
  -- where greeting :: String   <-- local function: would not compile because this "greeting" wouldn't be visible from "printSecond"
  --       greeting = "Yarrrrrd Stick"

-- global function: visible for both callers
greeting :: String 
greeting = "Yarrrrrd Stick"
