module Kinds where

import Data.Bool

{-
 
  A higher kinded type type is any type whose kind has a function arrow in it 
  and which can be described as a type constructor rather than a type constant. 
   
   The following types are of a higher kind than *:
  
    Maybe :: * -> *
    [] :: * -> *
    Either :: * -> * -> *
    (->) :: * -> * -> *

   The following are not:
  
    Int :: *
    Char :: *
    String :: *
    [Char] :: *

  Deducing kinds from type signatures:

  type signature:   id :: a -> a

  The kind of a type variable is denoted by *, which represents the kind of all types. 
  This means that a is a type variable that can be instantiated with any concrete type.
  The a type variable appears as a parameter and a return type, 
  indicating that the id function can take an argument of any type a and return a value of the same type. 
  This makes a a polymorphic type variable.
  Its kind is *


  type signature:  r :: a -> f a

  Here, a and f have specific kinds:
  a has kind *
  This means that a is a type variable that can be instantiated with any concrete type.

  f has a higher-kinded kind: 
  The kind of f is * -> *, which means that it's a type constructor that takes one type argument to produce a fully realized type.
  Otherwise stated, f is a type constructor that can be applied to a type to produce another type.

-}

-- =================================== Exercise using Either ===================================

notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe xs    = Just xs

replaceThe :: String -> String
replaceThe ss = replace $ words ss
 where
  replace [] = []
  replace (x:xs) = 
   let current = case (notThe x) of
                  Just t -> t
                  Nothing -> "a"
   in current ++ " " ++ replace xs
  
replaceThe' :: String -> String
replaceThe' [] = []
replaceThe' (' ':xs) = ' ' : replaceThe' xs -- skip evaluation of the space left by the preceding takeWhle operation 
replaceThe' xs = 
 let h       = takeWhile (\c -> c /= ' ') xs
     t       = dropWhile (\c -> c /= ' ') xs
     current = case (notThe h) of
                Just w -> w
                Nothing -> "a"
     in current ++ replaceThe' t 

countArticleBeforeVowel :: String -> Integer 
countArticleBeforeVowel xs = countArt 0 $ words xs
 where 
  countArt acc [] = acc
  countArt acc ("an":ys) = countArt acc ("the":ys)  
  countArt acc ("a":ys) = countArt acc ("the":ys)  
  countArt acc ("the":ys:zs) 
   | ys!!0 `elem` "aeiuo" = countArt (acc + 1)  zs  
   | otherwise =  countArt acc  zs
  countArt acc (_:xs)  =  countArt acc xs              


countArticleBeforeVowel' :: String -> Integer 
countArticleBeforeVowel' xs = countArt 0 $ words xs
  where 
   countArt acc [] = acc
   countArt acc (xs:ys:zs) 
    | xs `elem` ["a", "an", "the"] = 
     if (ys!!0 `elem` "aeiuo") 
     then countArt (acc + 1) zs
     else countArt acc zs 
   countArt acc (_:xs)  =  countArt acc xs  



countVowels :: String -> Integer 
countVowels xs = countVs 0 (words xs)
 where 
  countVs acc [] = acc
  countVs acc (xs:ys)  = let vs = fromIntegral $ length $ filter (flip elem "aeiou") xs 
                         in countVs (acc + vs) ys


