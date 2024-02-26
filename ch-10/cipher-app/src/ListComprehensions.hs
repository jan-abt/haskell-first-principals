module ListComprehensions where

import Data.Bool
import Data.Char
--
-- LIST COMPREHENSIONS
--                      [ x^2   |  x  <- [1..10], rem x 2 == 0 ] 
--                        [1]  [2]    [   3    ]i [     4     ]
-- 
--  From a list of numbers 1-10, take (<-) each element as an input to the output function (x^2).
--
--  1. This is the output function that will apply to each member of the list we indicate.
--  2. The pipe here designates the separation between the output function and the input.
--  3. One or more Generator lists, an input set and a variable that represents the elements that will be drawn from that list. 
--  4. One or more optional predicates.
--
--  If we use multiple generators, the rightmost generator will be exhausted first, then the second rightmost, and so on.
--  [ (x,y,z) | x <- [1,2,3], y <- [20, 30], z <- [40,50] ]
--  [ (1,20,40), (1,20,50), (1,30,40), (1,30,50), (2,20,40), (2,20,50), (2,30,40), (2,30,50), (3,20,40), (3,20,50), (3,30,40), (3,30,50) ]
--
--  LISTS
--
--      1 : 2 : 3 : []
--
--  OR
--
--      1 : (2 : (3 : []))
--
--  OR
--
--      :
--     / \
--    1   :
--       / \
--      2   :
--         / \
--        3  [ ]
--
--  The 1 : (2 : (3 : [])) representation makes it seem like the value 1 exists “before” the cons (:) cell that contains it, 
--  but actually, the cons cells contain the values. Because of this and the way non-strict evaluation works, 
--  you can evaluate cons cells independently of what they contain. 
--  It is possible to evaluate just the spine of the list without evaluating individual values.
--
--
--     : <------|
--    / \       |
--   _   : <----| This is the "spine"
--      / \     |
--     _   : <--|
--        / \
--       _  [ ]
--
-- EVALUATION
--  
--   mySum :: Num a => [a] -> a
--   mySum [] = 0
--   mySum (x : xs) = x + mySum xs
--
--   mySum [1..5]
--  
--   1 + (2 + (3 + (4 + (5 + 0))))
--   1 + (2 + (3 + (4 + 5)))
--   1 + (2 + (3 + 9))
--   1 + (2 + 12)
--   1 + 14
--   15


-- FILTER EXAMPLES
   
filterArticles :: String -> [String]
filterArticles xs = [w | w <- words xs, (elem  w ["The", "the", "A", "a", "An", "an"]) == False]

seqOfMultiples :: Int -> Int -> [Int]
seqOfMultiples multiplicant limit = [el | el <- (take limit [1..]), ((el `mod` multiplicant) == 0)]

collectMultiplesByOffset :: (Foldable t, Integral a) => a -> Int -> t a -> [a]
collectMultiplesByOffset n l os =  [el | el <- take l [1..], (elem (el `mod` n) os) ]


-- MAP EXAMPLE
oddEvenTuples :: [Int] -> [(Int, String)]
oddEvenTuples xs = map (\x -> (bool (x,"even") (x,"odd") (x `mod` 2 > 0) ) ) xs


-- ZIP UNZIP ZIPWITH

simpleZip = zip [1, 2, 3] [4, 5, 6]

simpleUnzip = unzip simpleZip

lowerBoundZip'  = zip [1, 2] [4, 5, 6]

lowerBoundZip''  = zip [] [1..1000000000000000000]

lowerBoundZip''' = zip ['a'] [1..1000000000000000000]


zip' :: [a] -> [b] -> [(a, b)]
zip' as bs = result as bs []
  where 
       result [] _ zs = zs
       result _ [] zs = zs
       result (x:xs) (y:ys) zs = result xs ys (zs++[(x,y)])


zip'' :: [a] -> [b] -> [(a, b)]
zip'' as bs = result as bs []
  where
       result (x:xs) (y:ys) zs =
         let pred = ( (length xs) == 0 ||  (length ys) == 0 ) 
             merged =  (zs++[(x,y)])
             zipNext =  result xs ys merged
             finished  = merged
         in bool zipNext finished pred

-- usage: zipWith' (\a b -> ( (show a) ++ "*" ++ (show b) ++ "=", (a*b) ) ) [1, 2, 3] [4, 5, 6]
--
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs = result as bs []
  where
       result [] _ zs = zs
       result _ [] zs = zs
       result (x:xs) (y:ys) zs = result xs ys (zs++[ f x y])

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f as bs = result as bs []
  where
       result (x:xs) (y:ys) zs = 
         let pred = ( (length xs) == 0 ||  (length ys) == 0 ) 
             appliedAndMerged =  (zs ++ [f x y])
             zipNext =  result xs ys appliedAndMerged
             finished  = appliedAndMerged
         in bool zipNext finished pred
 

-- ZIP AND CHARS

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = let continue  = capitalize xs
                        done      = (toUpper x):xs 
                        predicate = x == ' '
                     in bool done continue predicate

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = (toUpper x):(capitalizeAll xs)


firstCharAsUpper :: String -> Char
firstCharAsUpper = toUpper . head -- point free style (due to type level declaration, the string parameter can be inferred by the compiler)


