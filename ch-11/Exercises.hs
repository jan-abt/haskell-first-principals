module Exercises  where

import Data.Time
data DatabaseRecord = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                 deriving (Eq, Ord, Show)

records:: [DatabaseRecord]
records = [
  DbDate (UTCTime (fromGregorian 2023 11 2) (secondsToDiffTime 0)),
  DbNumber 1 ,
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 0)), 
  DbNumber 5 ,
  DbDate (UTCTime (fromGregorian 1971 7 5) (secondsToDiffTime 0)),
  DbString "Hello, world!", 
  DbNumber 3 ,
  DbNumber 9, 
  DbNumber 8 ,
  DbNumber 2 ,
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 0)),
  DbNumber 6 ,
  DbNumber 4,
  DbNumber 7
  ]


filterDbDate :: [DatabaseRecord] -> [UTCTime]
filterDbDate xs = collect xs []
  where 
    collect [] ds = ds
    collect (x:xs) ds = case x of
      DbDate d -> collect xs (d : ds)
      _ -> collect xs ds

filterDbNumber :: [DatabaseRecord] -> [Integer]
filterDbNumber xs = collect xs []
   where 
     collect [] ns = ns
     collect (x:xs) ns = case x of
       DbNumber n -> collect xs ((n::Integer) : ns)
       _ -> collect xs ns         


mostRecent :: [DatabaseRecord] -> UTCTime
mostRecent xs = head $ sort (filterDbDate xs)
  where sort [] = []
        sort (d:ds) = let newer = [s | s <- ds, s >= d] 
                          older = [s | s <- ds, s < d]
                      in (sort newer) ++ [d] ++  (sort older)

sumDb :: [DatabaseRecord] -> Integer
sumDb xs = foldr (+) 0 $ filterDbNumber xs                     


avgDb :: [DatabaseRecord] -> Double 
avgDb xs = let items = filterDbNumber xs
               total = fromIntegral $ length items
               sum   = fromIntegral $ foldr (+) 0 items
               average = sum / total
           in  average


--
-- ==================================================================
--

-- infinite 
fibs = 1:scanl (+) 1 fibs

fact = scanl (*) 1 [2,3..]

first20 =  [ e | e <- take 20 fibs]

lessThan100 =   [ e | e <- takeWhile (<100) fibs] 

vowelStopCombinations :: [String]
vowelStopCombinations = let stops        = "pbtdkg"
                            vowels       = "aeiou"
                            combinations = map (\(x, y, z)  ->  [x,y,z]) [(x,y, z) | x <- stops, y <- vowels , z <- stops]
                        in  foldr (\(e:es) acc -> if (e == 'p') then (e:es):acc else acc) [] combinations

nounVerbNounCombinations :: [String] -> [String] -> [(String, String, String)]
nounVerbNounCombinations ns vs = 
  let combinations = [(x,y,z) | x <- ns, y <- vs , z <- ns] 
  in  foldr (\(e, f, g) acc -> if (e /= g) then (e,f,g):acc else acc) [] combinations

avgWordLength sentence =  
  let totalChracters = fromIntegral (sum $ map length $ words sentence) 
      totalWords = fromIntegral . length . words $ sentence
  in totalChracters / totalWords   

myAnd :: [Bool] -> Bool 
myAnd = foldr (&&) True

myAnd' :: [Bool] -> Bool 
myAnd' = foldr (\a b ->if a == False then False else b) True

myOr :: [Bool] -> Bool
myOr bs = foldr (||) False bs

myOr' :: [Bool] -> Bool
myOr' bs = foldr (\e a -> if(e == False && a == False) then False else True) False bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f bs = foldr (\e a -> if(f e) then True else a) False bs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem q (e:es) = 
  if (e == q) 
      then True
      else myElem q es

myElem' :: Eq a => a -> [a] -> Bool
myElem' q es = foldr (\e a -> if(e == q) then True else a ) False es

myElem'' :: Eq a =>  (a -> Bool) -> [a] -> Bool
myElem'' f  =  myAny f

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myMap :: (a -> b) -> [a] -> [b] 
-- myMap f xs = foldr (\e a -> (f e):a ) [] xs
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter f xs  = foldr (\e a -> if (f e) then e:a else a) [] xs

squish :: [[a]] -> [a]
squish xss =  foldr (\e a -> e++a) [] xss

squishMap :: (a -> [b]) -> [a] -> [b] 
-- squishMap f xs = foldr (\e a -> (f e)++a) [] xs
squishMap f = foldr ((++) . f ) [] 

squishAgain :: [[a]] -> [a]
squishAgain xss = squishMap id xss

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f (x:xs) = foldl (\a e -> if(f a e == GT) then a else e) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy f (x:xs) = foldl (\a e -> if(f a e == LT) then a else e) x xs


