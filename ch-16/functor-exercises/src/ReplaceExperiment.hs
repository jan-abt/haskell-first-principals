module ReplaceExperiment where 

-- NESTED OPS
-- map (map (*10)) [[10]] | (map . map) (*10) [[10]]
-- This expression involves nested map operations. 
-- The outer mosst map applies a function,  (map (*10)) to each element of the outer list.
-- The inner map applies the function (*10) to each element of the inner lists.

-- Here's the step-by-step breakdown:

-- Outer map: map (map (*10)) [[10]]
-- Applies the function (map (*10)) to the first and only element [10].
-- Result: [map (*10) [10]]

-- Inner map: map (*10) [10]
--  Applies the function (*10) to the single element 10.
-- Result: [10 * 10] or [100]

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]
    
replaceWithP :: b -> Char
replaceWithP = const 'p' 
-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char 
replaceWithP' = replaceWithP

-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char
liftedReplace :: Functor f => f a -> f Char 
liftedReplace = fmap replaceWithP
--But we can assert a more specific type for liftedReplace! 
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char) 
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char) 
twiceLifted = (fmap . fmap) replaceWithP
-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char] 
twiceLifted' = twiceLifted
--f ~[]
-- f1 ~ Maybe

--Thrice?
-- Prelude> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
-- :: (Functor f2, Functor f1, Functor f) =>
-- f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char)) 
thriceLifted = (fmap . fmap . fmap) replaceWithP
-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]] 
thriceLifted' = thriceLifted
--f ~[]
-- f1 ~ Maybe
-- f2 ~ []

main :: IO () 
main = do
    putStr "replaceWithP'  lms:  "
    print (replaceWithP' lms)         
    putStr "liftedReplace  lms:  "
    print (liftedReplace lms)                            
    putStr "liftedReplace' lms:  "
    print (liftedReplace' lms)
    putStr "twiceLifted    lms:  "
    print (twiceLifted lms)
    putStr "twiceLifted'   lms:  "
    print (twiceLifted' lms)
    putStr "thriceLifted   lms:  "
    print (thriceLifted lms)
    putStr "thriceLifted'  lms:  "
    print (thriceLifted' lms)
