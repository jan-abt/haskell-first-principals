module Exercises where

import Data.Char
import Data.List hiding (isSubsequenceOf)

-- True if all the values in the first list appear in the second list.
-- They need not be contiguous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _  = True
isSubsequenceOf (e:es) seq@(_:qs) = 
 if(e `elem` seq) 
     then isSubsequenceOf es qs 
     else False

-- Splits a sentence into words, then tuples each, with its capitalized form as the second item of the tuple.
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map tuppelizeIt $ words xs
 where tuppelizeIt wd@(ch:chs) = (wd, (toUpper ch):chs ) 

-- capitalizes a word.
capitalizeWord :: String -> String 
capitalizeWord (c:cs) = (toUpper c):cs

--  Capitalizes sentences in a paragraph. 
--  Recognizes when a new sentence has begun by checking for periods. 
--  Reuses the capitalizeWord function
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p  = let sentence    =  takeWhile (\c -> c /='.') p 
                             rest        =  dropWhile (\c -> c /= '.') p
                             -- home made solution to inject whitespace between words, except infront of the 1st
                             (_:wd):capitalized = map (\s -> if s /= "." then " " ++ capitalizeWord s else s ++ " ") $ (words sentence)++["."] 
                         in concat (wd:capitalized) ++ capitalizeParagraph (tail rest)
                              -- using the standard library's "interlacate" function is more succinct
 --                          capitalized = map capitalizeWord $ words sentence
 --                      in (intercalate " " capitalized) ++ ". " ++ (capitalizeParagraph $ tail rest)
