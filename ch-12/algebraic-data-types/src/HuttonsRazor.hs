module HuttonsRazor where

import Data.List (sort)
import qualified Text.Show as GHC (show)

data Expr = 
 Lit Integer
 | Add Expr Expr

-- our own Show instance for Expr  
instance Show Expr where
 show (Lit v)   = GHC.show v
 show (Add l r) = show $ eval $ Add l r

eval :: Expr -> Integer 
eval (Lit n)   = n
eval (Add l r) = eval l  +  eval r

-- render the Expr as a mathematical term
printExpr :: Expr -> String
printExpr (Lit v)    = show v
printExpr (Add l r)  = printExpr l ++ " + " ++ printExpr r

-- usage samples:
-- eval (Add (Lit 1) (Lit 9001))
--
-- printExpr (Add (Lit 1) (Lit 9001))
--
-- let a1 = Add (Lit 9001) (Lit 1)
-- let a2 = Add a1 (Lit 20001)
-- let a3 = Add (Lit 1) a2
-- printExpr a3

--
-- =============================================================
--


data Tree a   =  
 Empty
 | Leaf a
 | Node (Tree a) a (Tree a)
 deriving (Show)

toList :: Tree a  -> [a]
toList Empty = []
toList (Leaf a) = [a]
toList (Node l v r) = toList l ++ [v] ++ toList r 

toTree :: [a] -> Tree a
toTree (x:y:z:[]) = Node (Leaf x) (y) (Leaf z)
toTree (x:y:[])   = Node (Leaf x) (y) (Empty)
toTree (x:[])     = Node (Empty) (x) (Empty)
toTree []         = Empty
toTree (x:xs)     = let m     = (length xs) `div` 2
                        (l,r) = splitAt m xs
                    in Node (toTree l) x (toTree r)

depth :: (Ord d, Num d) => Tree a -> d
depth a = traverse a 0
 where
    traverse Empty level        = level
    traverse (Leaf _) level     = level
    traverse (Node lft _ rgt) level = let lvl    = level + 1 
                                          lftLvl = traverse lft lvl                              
                                          rgtLvl = traverse rgt lvl
                                      in  max lvl (max lftLvl rgtLvl)

maxVal :: (Ord a, Num a) => Tree a -> a
maxVal a = traverse a 0
 where
    traverse Empty maxVal        = maxVal
    traverse (Leaf a) maxVal     = if (a > maxVal) then a else maxVal
    traverse (Node l v r) maxVal = let leftMax  = traverse l maxVal
                                       val      = if (v > maxVal) then v else maxVal
                                       rightMax = traverse r maxVal
                                    in max val (max leftMax rightMax)

-- usage samples:
--
-- sort . toList . toTree $ [1..17]
--
-- sort . toList . toTree . words $ "Today is Thursday, the 9th of November 2023. I will read chapter 12, entitled 'Signaling Adversity'"
--
-- let nTree = toTree [1,4,8,5,17,11,16,2,6,12,7,3,15,9,13,14,10] 
--
-- maxVal nTree
--
-- depth nTree
--
-- toList nTree
--
