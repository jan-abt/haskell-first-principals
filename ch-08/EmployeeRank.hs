module EmployeeRank (Employee (..), employeeRank) where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

isBossOf :: Employee -> Employee -> IO () 
isBossOf p e = putStrLn $ show p ++ " is the boss of " ++ show e

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO () 
employeeRank f e e' =
        case f e e' of
          GT -> isBossOf e e'
          EQ -> putStrLn "Neither employee is the boss" 
          LT -> (flip isBossOf) e e'

 -- usage:  employeeRank compare Veep CEO         
