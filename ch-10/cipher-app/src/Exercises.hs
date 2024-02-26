module Exercises where 

import Data.Bool

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs

-- returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool 
myOr bs = bool False True  (elem True bs)


-- returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _  [] = False 
myAny f (x:[]) = f x 
myAny f (x:xs) = bool (myAny f xs) True (f x) 

-- returns True if the specified element exists in the list.
myElem :: Eq a => a -> [a] -> Bool
myElem el [] = False
myElem el (x:xs) = bool (myElem el xs) True (x == el)

myReverse :: [a] -> [a]
myReverse xs = rev xs []
  where 
       rev [] rs = rs
       rev (x:xs) rs = rev xs (x:rs)

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs)  = (myReverse' xs)++[x]

--  flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

-- maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f [] = []
squishMap f (x:xs) = f x ++ (squishMap f xs)
-- usage:  squishMap (\x -> [(x, chr x)]) [1,2..5]


squishAgain :: [[a]] -> [a] 
squishAgain xs = squishMap id xs 


maximumBy' ::  (a -> a -> Ordering) -> [a] -> a
maximumBy' f (x:xs)  =  maxBy f (x:xs) x 
  where
     comparator f a b = if GT == (b `f` a) then b else a  
     maxBy f (x:[]) m = comparator f x m 
     maxBy f (x:xs) m = maxBy f xs $ comparator f x m

-- usage: 
-- maximumBy' (compare `on` length) ["Hello", "World", "!", "Longest", "bar"]
-- maximumBy' (compare `on` id) [2,4,7,8,7,4]
-- maximumBy' compare  [2,4,7,8,7,4]
-- maximumBy' (\x y -> bool LT GT (x > y) ) [2,4,7,8,7,4]
-- maximumBy' (\_ _ -> GT) [1..10]
-- maximumBy' (\_ _ -> LT) [1..10]
-- maximumBy' compare [1..10]

minimumBy' ::  (a -> a -> Ordering) -> [a] -> a
minimumBy' f (x:xs)  =  minBy f (x:xs) x
  where
     comparator f a b = if LT == (b `f` a) then b else a
     minBy f (x:[]) m = comparator f x m
     minBy f (x:xs) m = minBy f xs $ comparator f x m

-- usage:
-- minimumBy' (compare `on` length) ["Hello", "World", "!", "Longest", "bar"]
-- minimumBy' (\x y -> bool LT GT (x > y) ) [2,4,7,8,7,4]
-- minimumBy' (\_ _ -> GT) [1..10]
-- minimumBy' (\_ _ -> LT) [1..10]
-- minimumBy' compare [1..10]

