
module
    UTCTimeConversions
        where

import Data.Time.Clock
import Data.Time.Format
import Data.Time

-- =========================== UTILITIES ========================================= --    

-- given a Day, this constructs an instance of the 
-- UTCTime data type at Zero hours, minutes and seconds    
timeFromDay :: Day -> UTCTime
timeFromDay day = UTCTime day (timeOfDayToTime midnight)

-- given a NominalDiffTime, this constructs an instance of the UTCTime data type, 
-- with the current time taken from the system clock, plus the specified time offset
timeFromOffset :: Maybe NominalDiffTime -> IO UTCTime
timeFromOffset localeOffset =
    case localeOffset of
        Just hours -> addUTCTime ((hours + 24) * 3600) <$> getCurrentTime -- performs IO Action
        _ -> getCurrentTime

-- hours, minutes and (pico) seconds
timeOfDayFromTime:: UTCTime -> TimeOfDay
timeOfDayFromTime = timeToTimeOfDay . utctDayTime

-- advance the given UTCTime by the number of hours and minutes specified
advanceTime :: NominalDiffTime -> NominalDiffTime -> UTCTime -> UTCTime
advanceTime h m = addUTCTime ((h*60*60)+(m*60))

-- given a yyyy-mm-dd formatted string,
-- this function constructs an instance of the Day data type with 0 hours, minutes and seconds
dayFromString :: String -> Maybe Day
dayFromString = parseTimeM True defaultTimeLocale "%0Y-%0m-%0d"

main :: IO ()
main = do

    putStrLn "======="

    timeFromOffset (Just 7) >>= \t -> putStrLn $ "Time from offset (Just 7): " ++ show t
    let maybeDay = dayFromString "2024-12-12"
    putStrLn $  "Date from string : "++ show maybeDay
    let atMidnight = timeFromDay <$> maybeDay
    putStrLn $  "UTC Time from Day: " ++ show atMidnight
    let tm = advanceTime 10 15 . timeFromDay <$> maybeDay
    putStrLn $ "UTC Time Advanvanced forward by hrs & mins (10 15) : "++  show tm
    let together = advanceTime 10 30 . timeFromDay <$> dayFromString "2024-12-12"
    putStrLn $ "Combination of all of the above: "++ show together
    putStrLn $ "Time of day from UTC Time" ++  show (timeOfDayFromTime <$> tm)

    putStrLn "======="


