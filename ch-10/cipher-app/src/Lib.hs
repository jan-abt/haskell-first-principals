module Lib where

import Data.Bool

--          -- --------------- --
--          -- Vigenère Cipher --
--          -- --------------- --
--      (builds on the "Caesar" cipher)

expKey :: String -> Int ->String
expKey xs n = take n $ expand xs []
 where expand [] c = expand xs c
       expand (x':xs') c = x':expand xs'  c 

data Direction = LTR | RTL deriving (Eq)

-- a raster of all permitted characters over which shift operations can take place.
-- raster LTR == " ,.!abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- raster RTL == "ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmlkjihgfedcba!., "
raster:: Direction -> [Char]
raster direction = bool (['Z','Y'..'A']++['z','y'..'a']++['!','.',',',' ']) ([' ',',','.','!']++['a'..'z']++['A'..'Z']) (LTR == direction)

indexAtRaster :: Char -> Int
indexAtRaster c = 
 let indexedRaster = zipWith (\a b -> (a,b)) (raster LTR) [1,2..]
 in head [snd t| t<-indexedRaster, fst t == c]

-- This function performs right shift operations of a character over the raster, beginning at the point where it's character matches the one that was given.
-- The shift amount is indicated by the given offset parameter.
-- The character that sits at that final position of the raster, when the shifts are completed, is the character to be returned.
-- If the offset value is larger than the length of the raster, shifting will continue a the opposite end of the raster until 0 has been reached.
-- Even though shifts are always moving from left to right, the underlying order of the raster can change based on the value of the "Direction"  constructor.
-- 
-- shiftOverRaster LTR 56 'a' == 'a'
-- shiftOverRaster LTR 112 'a' == 'a'
-- shiftOverRaster LTR 57 'a' == 'b'
-- shiftOverRaster LTR 113 'a' == 'b'
--
-- shiftOverRaster RTL 56 'a' == 'a'
-- shiftOverRaster RTL 112 'a' == 'a'
-- shiftOverRaster RTL 55 'a' == 'b'
-- shiftOverRaster RTL 111 'a' == 'b'

shiftOverRaster :: Direction -> Int -> Char -> Char
shiftOverRaster direction offset char = 
  shiftr offset $ tail . dropWhile (/= char) $ raster direction
  where
     shiftr 1 (c:_) =  c
     shiftr o []     =  shiftr o $ raster direction
     shiftr o (_:cs) =  shiftr (o-1) cs

-- Each character of the password string is converted into an offset value, resulting in an offset value list.
-- That list is repeated/concatenated until it is expanded enough to have it's length match that of the text to be encoded.
-- Each letter of the raw text takes the next available offset value from the list to perform its LTR shift operation over the raster, resulting in the encoded text.
encode :: String -> String ->  String
encode _ [] = []
encode ks xs = steps xs (expKey ks (length xs))
 where
   steps [] _ = [] 
   steps xs' [] =  encode ks xs'
   steps (x:xs') (k:ks') = (shiftOverRaster LTR (indexAtRaster k) x):(steps ks' xs') 

-- Each character of the password string is converted into an offset value, resulting in an offset value list.
-- That list is repeated/concatenated until it is expanded enough to have it's length match that of the text to be encoded.
-- Each letter of the encoded text takes the next available offset value from the list to perform its RTL shift operation over the raster, resulting in the decoded text.
decode :: String -> String ->  String
decode _ [] = []
decode ks xs = steps xs (expKey ks (length xs))
 where
   steps [] _ = [] 
   steps xs' [] =  decode ks xs' 
   steps (x:xs') (k:ks') = (shiftOverRaster RTL (indexAtRaster k) x):(steps xs' ks')

--  examples for encode/decode functionality checks:
--  decode "mylongsecretpassword" . encode "mylongsecretpassword" $ "have a nice day."
--  decode "shrtpwd" . encode "shrtpwd" $ "hello fellows. have a nice day."




