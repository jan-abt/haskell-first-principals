module Folds
    ( 
    ) where


import Data.List

--      --------------------------
--      ---------  GENERAL -------
--      --------------------------
--
-- foldr
--  1. The rest of the fold (recursive invocation of foldr) 
--     is an argument to the folding function you passed to foldr. 
--     It doesn’t directly self-call as a tail-call like foldl. 
--     You could think of it as alternating between applications of foldr and your folding function f. 
--     The next invocation of foldr is conditional on f having asked for more of the results of having folded the list. 
--     That is:
--        foldr :: (a -> b -> b) -> b -> [a] -> b
--                    -- ^
--     That "b" is the rest of the fold. 
--     Evaluating that evaluates the next application of foldr.
--  2. Associates to the right.
--  3. Works with infinite lists. 
--  4. Is a good default choice whenever you want to transform data structures, be they finite or infinite.
--
-- foldl
--  1. Self-calls (tail-call) through the list, only beginning to produce values after it’s reached the end of the list.
--  2. Associates to the left.
--  3. Cannot be used with infinite lists.
--  4. Is nearly useless and should almost always be replaced with foldl'.
--
--
-- parallels etween map & foldl, foldr
--
--   map ::      (a -> b) ->       [a] -> [b]
-- foldr :: (a -> b -> b) ->  b -> [a] ->  b
--
--
-- `map` applies the function parameter to each member of a list and returns a list.
--
--  map (+1) 1  :       2 :       3 : []
--      (1 + 1) : (1 + 2) : (1 + 3) : []
--
--
-- `foldr` replaces the cons constructors of it's list with the function parameter and reduces the list.
--
--  foldr (\x y -> x + y) 0 (1 :  2 :  3 : [])
--                          (1 + (2 + (3 + 0)))
--
--      -------------------------
--      ------  FOLD RIGHT ------
--      -------------------------
--
--
--  foldr :: (a -> b -> b) -> b -> [a] -> b 
--  foldr f acc [] = acc
--  foldr f acc (x:xs) = f x (foldr f acc xs)
--  
--  Note the similarities between foldr implementation and the recursive patterns.
--  The "tail" or "rest" of the foldr, i.e. the "(foldr f acc xs)" part 
--  is an argument to the function f we’re folding with.
--
--  foldr f acc [1, 2, 3] = 
--  1.   f 1 (foldr f acc [2, 3])
--  2.   f 1 (f 2 (foldr f acc [3]))
--  3.   f 1 (f 2 (f 3 (foldr f acc [])))
--
--  Right folds are right associative
--  The expression in the innermost parentheses is evaluated first.
--  The folding itself starts at the end of the list.
--
--     f 1 (f 2 (f 3 acc))
--
--  EXAMPLES:
--
--  add :: Num a => a -> a -> a
--  add a b = a + b  
--
--  foldr add 5 [1,2,3] =  (add 1 (add 2 (add 3 5))) 
--                         (add 1 (add 2      8  )) 
--                         (add 1     10        ) 
--                          11
--  
--  foldr (^)  2  [1..3] = (1 ^ (2 ^ (3 ^ 2)))
--                         (1 ^ (2 ^ 9)) 
--                          1 ^ 512
--                          1
--
--      TRY THIS VISUALIZATION IN THE REPL:
--      let f = (\el acc -> concat ["(",el,"+",acc,")"])
--      foldr f "0" (map show [1..5])
--
--
--      -------------------------
--      ------- FOLD LEFT -------
--      -------------------------
--
--  foldl :: (b -> a -> b) -> b -> [a] -> b 
--  foldl f acc [] = acc
--  foldl f acc (x:xs) = foldl f (f acc x) xs
--
--  Left folds traverse the spine in the same direction as right folds,
--  but their folding process is left associative and proceeds in the opposite direction as that of foldr.
--
--    foldl f (f acc x) xs
--
--  foldl f acc [1, 2, 3] = 
--  1.  foldl f (f acc 1) [2, 3]
--  2.  foldl f (f (f acc 1) 2) [3]
--  3.  foldl f (f (f (f acc 1) 2) 3) []
--
--  Left Folds are left associative.
--  The expression in the innermost parentheses is evaluated first.
--  The folding itself starts at the beginning of the list.
--
--     f (f (f acc 1) 2) 3)
--
--
--  EXAMPLES: 
--
--  add :: Num a => a -> a -> a
--  add a b = a + b  
--
--  foldl add [1,2,3] =  (add (add (add 5 1) 2) 3)
--                       (add (add        6  2) 3)
--                       (add                8  3)
--                                             11                       
--  foldl (^) 2 [1..3] = ((2 ^ 1) ^ 2) ^ 3 
--                        (2 ^ 2) ^ 3
--                         4 ^ 3
--                        64
--
--      TRY THIS VISUALIZATION IN THE REPL:
--      let f = (\acc el -> concat ["(",acc,"+",el,")"])
--      foldl f "0" (map show [1..5])
--
--
--  -----------------------------------------------------------------------------------------    
--  -----------------------------------------------------------------------------------------  
--  -----------------------------------------------------------------------------------------  
--
--

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f acc [] = acc
foldR f acc (x:xs) = f x (foldR f acc xs) 

-- EXAMPLES
--   1 + (2 + (3 + 2)) = 8
--   1 ^ (2 ^ (3 ^ 2)) = 1
--  
--  usage:  foldR (:) [] [1..5] = [1,2,3,4,5]

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL f acc [] = acc
foldL f acc (x:xs) = foldL f (acc `f` x) xs

--  EXAMPLES
--    ( (2 + 1) + 2) + 3 = 8
--    ( (2 ^ 1) ^ 2) ^ 3 = 64
--
-- usage: foldL (flip (:)) [] [1..5] = [5,4,3,2,1]

