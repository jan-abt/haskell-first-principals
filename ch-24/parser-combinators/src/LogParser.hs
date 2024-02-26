{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module
    LogParser
        where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ ( r )
import Data.Time.Format
import Data.Time
import Data.Maybe ( catMaybes )
import qualified Data.Map.Strict as S
import Data.List
import Data.Fixed

newtype Schedule = Schedule {
     itineraries :: [Itinerary]
}    deriving (Eq, Show)

data Itinerary = Itinerary {
    day::Day,
    activities::[Activity]
} deriving (Eq, Show)

data Activity = Activity {
    start::TimeOfDay,
    duration::Maybe TimeSpent,
    description::Description
} deriving (Eq, Show)

type TimeSpent = TimeOfDay
type Description = String
type Comment = String

-- ========================================== PARSER UTILIY FUNCTIONS ================================================= --    

-- returns everything preceeding the marker 
keepPreceeding :: (Alternative m , Parsing m)=> m p -> m k -> m [p]
keepPreceeding parserM markerM = accumulatorM
    where
        accumulatorM =
                -- if successfully matched, return an parser-monad of [], thusly completing the list construction.
                -- use "try" to reset the pointer to the position where it was before the marker-monad was applied,
                try $ const [] <$> markerM
                <|>
                -- otherwise, apply the parser recursively, crunching the next char and                             
                -- append the result to the list parser-monad.                              
                liftA2 (:) parserM accumulatorM

-- drops everything preceeding the marker - returns the marker
dropPreceeding :: (Monad p ,Alternative p , Parsing p) => p w -> p m -> p m
dropPreceeding cruncher marker =
    try marker <|> (cruncher >> dropPreceeding cruncher marker)

-- given a yyyy-mm-dd formatted string,
-- this function constructs an instance of the Day data type
dayFromString :: String -> Maybe Day
dayFromString = 
    parseTimeM True defaultTimeLocale "%0Y-%0m-%0d"

-- ========================================== LOG PARSER FUNCTIONS ================================================= --

parseSchedule :: Parser (Maybe Schedule)
parseSchedule =
    let maybeSchedule =
                many parseItinerary >>=
                        \itnMaybes ->
                            let maybeItineraries = sequence itnMaybes
                            in return (Schedule <$> maybeItineraries)
    in  maybeSchedule

parseItinerary :: (Monad p, CharParsing p) => p (Maybe Itinerary)
parseItinerary =
    let maybeItinerary = parseDay >>=
            \maybeDay -> let activities =
                                catMaybes <$> many parseActivity
                         in activities >>=
                                \acts -> return (flip Itinerary (postInitializeDuration acts) <$> maybeDay )
    in maybeItinerary
    where
        postInitializeDuration :: [Activity] ->  [Activity]
        postInitializeDuration [] = []
        postInitializeDuration (x:xs) =
            reverse $ foldl (\ls curr ->
                let prev = head ls
                    durn = calculateDifference (start curr) (start prev)
                    prev' = Activity (start prev) (Just durn) (description prev)
                in  (curr:prev':tail ls)
            ) [x] xs
        calculateDifference :: TimeOfDay -> TimeOfDay -> TimeOfDay
        calculateDifference c p =
            let cs = (3600 * todHour c + 60 * todMin c)
                ps = (3600 * todHour p + 60 * todMin p)
                (hs, ms) = hmAsSecondsTuple (cs - ps)
                in
                    case (hs, ms)  of
                        (_, ms') | ms' < 3600 ->
                            let (ms'', _) = msAsSecondsTuple ms'
                            in TimeOfDay hs ms'' 0
                        _ -> TimeOfDay hs ms 0
            where
                hmAsSecondsTuple s =  (s `div` 3600, s `mod` 3600)
                msAsSecondsTuple s =  (s `div` 60, s `mod` 60)

parseDay :: (Monad p, CharParsing p) => p (Maybe Day)
parseDay =
        -- if we encounter a comment or an emplty line, skip and continue
        try (parseComment >> parseDay)
        <|>
        try (parseEmptyLine >> parseDay)
        <|>
        dayFromString <$> dayParser
        where
            dayParser =
                try (parseComment >> pure [])
                <|>
                 -- ignore previous characters and move cursor to after "# "
                 (many (char ' ' <|> char '\t') <* string "# " >>
                 -- collect the date string, such as for example 2020-12-12                                                                
                many (digit <|> char '-') <* many (char ' ' <|> char '\n'))

parseActivity :: (Monad p, CharParsing p) => p (Maybe Activity)
parseActivity  =
    try (parseComment >> pure Nothing )
        <|>
        (many (char ' ' <|> char '\t') >>
            parseTimeOfDay >>= \atTime ->
                let partialActivity =  Activity atTime Nothing
                in  Just . partialActivity <$> parseDescription <* many (oneOf " \n\t\r")
        )

parseTimeOfDay :: ( Monad p, CharParsing p) => p TimeOfDay
parseTimeOfDay = digit >>=
            \d1 -> digit >>=
                \d2 -> char ':' >> digit >>=
                    \d3 -> digit >>=
                        \d4 -> let hr  = read [d1,d2] :: Int
                                   mn  = read [d3,d4] :: Int
                               in return (TimeOfDay hr mn 0)

parseDescription ::(Monad p, CharParsing p) => p Description
parseDescription = 
    skipMany (oneOf " \t") >>
        noneOf "\n" `keepPreceeding` try (parseComment <|> string "\n" )

parseComment :: (Monad p, CharParsing p) => p Comment
parseComment =
    skipMany (oneOf " \t") >> string "--"  >> noneOf "\n" `dropPreceeding` string "\n"

parseEmptyLine :: (Monad p, CharParsing p) => p ()
parseEmptyLine =
   skipMany (oneOf " \t") <* char '\n'

-- ============================================ DOMAIN FUNCTIONS =============================================== --

extractItineraries :: Result (Maybe Schedule) -> [Itinerary]
extractItineraries (Success (Just sch)) = itineraries sch
extractItineraries (Success _ ) = []
extractItineraries (Failure (ErrInfo _ _)) = []

activitiesAndTimeSpent :: [Itinerary] -> S.Map Day[(Description, Maybe TimeSpent)]
activitiesAndTimeSpent its =
    let das =  [ (day it, activities it) | it <- its ]
    in foldr (\(k, as) m ->
        let  v = (\c -> (description c, duration c)) <$> as
        in S.insert k v m
    ) S.empty das

averageTimeSpent :: [Itinerary] -> [(Description, Either String TimeSpent) ]
averageTimeSpent is =
    let flattened = concat [activities i | i <- is]
        tupled = (\act ->     -- lift out of its Maybe context
            (description act, case duration act of Just v -> v; Nothing -> TimeOfDay 0 0 0)) <$> flattened
        grouped = groupBy (\(d1, _) (d2, _) ->  d1 == d2 ) $ sort tupled
        folded = foldGroups grouped
    in  folded
    where
        foldGroups :: [[(Description, TimeSpent)]] -> [(Description, Either String TimeSpent) ]
        foldGroups grps =
            (\xs -> case xs of
                      [(g,_)] -> (g, Left "N/A, Unique Occurrence")
                      (g:gs) ->
                        let (desc,tod) = 
                                foldr (\(d,t) (_,t') ->  (d, calculateSum t t' )  ) g gs
                        in  (desc, Right $ calculateAvg tod (length xs))                      
            ) <$> grps

        calculateSum :: TimeOfDay -> TimeOfDay -> TimeOfDay
        calculateSum c p =
                    let cs = (3600 * todHour c + 60 * todMin c)
                        ps = (3600 * todHour p + 60 * todMin p)
                        (hs, ms) = hmAsSecondsTuple (cs + ps)
                        in case (hs, ms)  of
                                (_, ms') | ms' < 3600 ->
                                    let (ms'', _) = msAsSecondsTuple ms'
                                    in TimeOfDay hs ms'' 0
                                _ -> TimeOfDay hs ms 0
                    where
                        hmAsSecondsTuple s =  (s `div` 3600, s `mod` 3600)
                        msAsSecondsTuple s =  (s `div` 60, s `mod` 60)

        calculateAvg :: TimeOfDay -> Int -> TimeOfDay
        calculateAvg c d =
                    let cs = (3600 * todHour c + 60 * todMin c)
                        (hs, ms) = hmAsSecondsTuple (cs `div` d)
                    in if ms < 3600
                       then
                            let (ms', ss) = msAsSecondsTuple ms
                            in if ss < 60 
                                then TimeOfDay hs ms' (fromIntegral ss::Pico)
                                else TimeOfDay hs ms' 0
                        else TimeOfDay hs ms 0
                    where
                        hmAsSecondsTuple s =  (s `div` 3600, s `mod` 3600)
                        msAsSecondsTuple s =  (s `div` 60, s `mod` 60)

-- using Quasiquotes, usage [r|  ... textual content ... |]
sampleLog :: String
sampleLog = [r| -- comment 1
    
    --  comment 2
        --  comment 3

 --  comment 4

    # 2025-02-05
    08:00 Breakfast
    09:00 Sanitizing moisture collector
    11:00 Exercising in high-grav gym
    12:00 Lunch
    13:00 Programming
    17:00 Commuting home in rover
    17:30 R&R
    19:00 Dinner
    21:00 Shower
    21:15 Read
    22:00 Sleep

    # 2025-02-07 -- omit this quote as well
    08:00 Breakfast -- skipping  breafast, some people do it
    09:30 Bumped head, passed out -- another comment, written a.d 2024-01-28        
    13:36 Wake up, headache
    13:37 Go to medbay
    13:40 Patch self up
    13:45 Commute home for rest
    14:15 Read
    21:00 Dinner
    -- 21:10 Answer Phone
    21:15 Read
    22:00 Sleep
|]

-- besides returning the scheduled itinerares, this function also 
-- has the intended "side effect" of printing the result to the terminal window
printAndExtractItineraries :: Result (Maybe Schedule) -> IO [Itinerary]
printAndExtractItineraries s =  do
    case s of
        q@(Success (Just sch)) -> do
            --putStrLn "\n=== extractValue :: Result (Maybe Schedule) -> IO (Maybe Schedule)  ===\n"
            putStrLn "\nOriginal query result:"
            print q
            return $ itineraries sch
        (Success _ ) -> do
            --putStrLn "\n=== extractValue :: Result (Maybe Schedule) -> IO (Maybe Schedule)  ===\n"
            print "\nNo Content:"
            pure []
        (Failure (ErrInfo e _)) -> do
            print "\nFailed With:"
            print e
            pure  []

main :: IO ()
main = do

    let parseResult = parseString parseSchedule mempty sampleLog
    putStrLn "\n\n -- The entire schedule --"
    print parseResult
    putStrLn "\n\n -- Restructured to show a map of activities and time spent --"
    print$ activitiesAndTimeSpent $ extractItineraries parseResult
        
    putStrLn "\n\n -- Average time spent on each activity, where applicable --"
    print $ averageTimeSpent $  extractItineraries parseResult
