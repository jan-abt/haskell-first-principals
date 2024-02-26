module DayOfWeek where
    
data DayOfWeek =  Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
 (==) Mon Mon   = True 
 (==) Tue Tue   = True
 (==) Weds Weds = True
 (==) Thu Thu   = True
 (==) Fri Fri   = True
 (==) Sat Sat   = True
 (==) Sun Sun   = True 
 (==) _   _     = False

-- Calendar Date: day of week and numerical day of month, no year
type DayOfMonth = Int
data Date = CalendarDate DayOfWeek DayOfMonth

-- note, that in the Eq instance for our Date data type, we didn’t have to recapitulate equality for DayOfWeek and DayOfMonth. 
-- we are stating that dates were equal, if all of their constituent values were equal.
instance Eq Date where
 (==) (CalendarDate dw dm) (CalendarDate dw' dm') = dw == dw' && dm == dm'


instance Show DayOfWeek where 
 show Mon  = "Monday"
 show Tue  = "Tueday"
 show Weds = "Wednesday"
 show Thu  = "Thursday"
 show Fri  = "Friday"
 show Sat  = "Satday"
 show Sun  = "Sunday" 

instance Show Date where
 show (CalendarDate d n ) | (not $ elem n [11,12,13]) && (mod n 10 == 1) = (show d) ++ ", the " ++ (show n) ++ "st"
                      | (not $ elem n [11,12,13]) && (mod n 10 == 2) = (show d) ++ ", the " ++ (show n) ++ "nd"
                      | (not $ elem n [11,12,13]) && (mod n 10 == 3) = (show d) ++ ", the " ++ (show n) ++ "rd"
                      | otherwise = (show d) ++ ", the " ++ (show n) ++ "th"

