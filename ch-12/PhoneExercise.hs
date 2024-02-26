module PhoneExercise 
-- (assocDigWithBtnPresses, assocCharsWithBtns, toCharacter) 
where

import Data.Char    
import Data.Bool
import Data.List

-- --------------------------
--  |1      |2 ABC |3 DEF  |
-- --------------------------
--  |4 GHI  |5 JKL |6 MNO  |
-- --------------------------
--  |7 PQRS |8 TUV |9 WXYZ |
-- --------------------------
--  |  *^   |0 +_  |# .,   |
-- --------------------------

type Options = String

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data Button = Button {
    digit::Digit,
    options::Options
} deriving (Show)

instance Eq Button where
 (==) (Button d _) (Button d' _) = d == d'   


data CB = CB{
     character::Char,
     button::Button
 } deriving (Show)

instance Eq CB where
 (==) cb cb' =  (character cb) == (character cb')

instance Ord CB where
 compare cb cb' = (character cb) `compare` (character cb')


one :: Button
one = Button '1' "1"

two:: Button
two = Button '2' "2ABC"

three:: Button
three = Button '3' "3DEF"

four:: Button
four = Button '4' "4GHI"

five:: Button
five = Button '5' "5JKL"

six:: Button
six = Button '6' "6MNO"

seven:: Button
seven = Button '7' "7PQRS"

eight:: Button
eight = Button '8' "8TUV"

nine:: Button
nine = Button '9' "9WXYZ"

star:: Button
star = Button '*' "*^"

zero:: Button
zero = Button '0' "0+_ "

pound:: Button
pound = Button '#' "#.,"

-- count the digit presses needed to reach the given character.
assocDigWithBtnPresses :: CB -> [(Digit, Presses)]
assocDigWithBtnPresses cb = let c = character cb
                                bt = button cb
                                d = digit bt
                                os = options bt
                            in    
                             case isUpper c of
                              True  -> [('*', 2), (d, presses c os)]
                              False -> [(d, presses c os)]
                            where 
                              presses c' os' = 
                                 if not $ (toUpper c') `elem` os'
                                  then error "digit does not exist on this button"
                                  else  (1 + (length $ takeWhile(\x -> x /= toUpper c') os' ))

-- converts a string of text to a list of tuples, each containing one character and its respective button.
assocCharsWithBtns :: String -> [CB]
assocCharsWithBtns xs = convert xs []
 where 
  convert [] acc = acc
  convert (c:cs) acc | toUpper c `elem` (options one)    = (CB c one):convert cs acc
                     | toUpper c `elem` (options two)    = (CB c two):convert cs acc  
                     | toUpper c `elem` (options three)  = (CB c three):convert cs acc
                     | toUpper c `elem` (options four)   = (CB c four):convert cs acc
                     | toUpper c `elem` (options five)   = (CB c five):convert cs acc
                     | toUpper c `elem` (options six)    = (CB c six):convert cs acc
                     | toUpper c `elem` (options seven)  = (CB c seven):convert cs acc
                     | toUpper c `elem` (options eight)  = (CB c eight):convert cs acc
                     | toUpper c `elem` (options nine)   = (CB c nine):convert cs acc
                     | toUpper c `elem` (options star)   = (CB c star):convert cs acc
                     | toUpper c `elem` (options zero)   = (CB c zero):convert cs acc
                     | toUpper c `elem` (options pound)  = (CB c pound):convert cs acc
                     | otherwise = acc

-- return a character based on the given seq of key instructions.
-- even though only one character is returned, a seq of key instructions is needed 
-- because capitals require two different buttons to be pressed.
toCharacter :: [(Digit, Presses)] -> Char
toCharacter [('*',2), dps] = toUpper $ toChar dps -- upper case
toCharacter [dps] = toLower $ toChar dps -- lower case


toString :: [(Digit, Presses)] -> String
toString [] = []
toString (('*',2):dp:dps) = (toUpper $ toChar dp): toString dps -- upper cas
toString (dp:dps) = (toLower $ toChar dp):toString dps -- lower case
 

toChar:: (Digit, Presses) -> Char      
toChar (dig, presses) | dig == (digit one) =  (options one)!!(presses-1)
toChar (dig, presses) | dig == (digit two) =  (options two)!!(presses-1)
toChar (dig, presses) | dig == (digit three) =  (options three)!!(presses-1)
toChar (dig, presses) | dig == (digit four) =  (options four)!!(presses-1)
toChar (dig, presses) | dig == (digit five) =  (options five)!!(presses-1)
toChar (dig, presses) | dig == (digit six) =  (options six)!!(presses-1)
toChar (dig, presses) | dig == (digit seven) =  (options seven)!!(presses-1)
toChar (dig, presses) | dig == (digit eight) =  (options eight)!!(presses-1)
toChar (dig, presses) | dig == (digit nine) =  (options nine)!!(presses-1)
toChar (dig, presses) | dig == (digit star) =  (options star)!!(presses-1)
toChar (dig, presses) | dig == (digit zero) =  (options zero)!!(presses-1)
toChar (dig, presses) | dig == (digit pound) =  (options pound)!!(presses-1)


asPressInstructions:: [String] -> [[(Digit, Presses)]]
asPressInstructions xs = concat $ map (\ms -> map assocDigWithBtnPresses . assocCharsWithBtns $ ms) xs


-- digits pressed for a single text message
fingerTaps :: [[(Digit, Presses)]] -> Presses
fingerTaps xs = sum $ taps xs
 where 
  taps [] = []
  taps (x:xs) =  foldr (\(_,p) a -> a + p) 0 x : taps xs

-- What was the letter most used of a message?
mostUsedInMessage :: [CB] -> Char
mostUsedInMessage [] = error "at least one letter is required"
mostUsedInMessage cbs = character $ head 
                                 $ foldr (\g a -> if((length g) > (length a)) then g else a) []
                                 $ group
                                 $ filter (\c -> character c /= ' ')
                                 $ sortBy (\l r -> (character l) `compare` (character r) ) 
                                 $ cbs
                       
-- What was the most used letter of all messages?
mostUsedOverAll :: [String] -> Char
mostUsedOverAll ms =  mostOverAll []  (group $ map (mostUsedInMessage . assocCharsWithBtns)  ms)
 where
   mostOverAll acc [] = foldr (\e a -> bool e a (e == a)) ' ' acc -- de-duplicate
   mostOverAll acc (m:ms) = 
     let mx = bool acc m ((length m) > (length acc)) -- store current max in acc
     in mostOverAll mx ms

pressInstructionsToText :: [[(Digit, Presses)]] -> [String]
pressInstructionsToText xs = 
     let arrayed = foldr (\e lss@(ls:slss) -> 
                if ( e /= ('0',4) ) 
                 then ((e:ls):slss) 
                 else []:lss 
               ) [[]] [x | x <- (concat xs), fst x /= '*']
 in map toString arrayed 
-- =============================================================================================

sampleMessages :: [String]
sampleMessages =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Aaaah, all assumed",
     "Hahaaaaaaaaa thanks just making sure rofl ur turn"]

-- round trip
-- there ...
-- let digitPresses = map (\xs -> map assocDigWithBtnPresses . assocCharsWithBtns $ xs) sampleMessages
-- ... and back
-- map (\xs -> map toCharacter xs) digitPresses
-- asPressInstructions sampleMessages :: [[(Digit, Presses)]]
-- coolestWord  $ asPressInstructions sampleMessages 






