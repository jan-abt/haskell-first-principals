module MaybeExercises where

import Data.List
import Data.Char
import Data.Bool

newtype WordWithMoreConsonantsThanVowels =
 WordWithMoreConsonantsThanVowels String 
 deriving (Eq, Show)

moreConsontantsThanVowels :: String -> Maybe WordWithMoreConsonantsThanVowels
moreConsontantsThanVowels xs = let (cons, vows) = foldr (\ch acc ->  partition ch acc) ("","") xs
            in bool Nothing (Just $ WordWithMoreConsonantsThanVowels xs) (length cons > length vows)
 where
  partition c a = bool (c:(fst a), snd a) (fst a, c:(snd a)) ( (toLower c) `elem` "aeiou" )

--
-- =========================================
--

-- whole numbers, from 0 to infinity
-- Succ (Succ (Succ Zero))
-- Succ $ Succ $ Succ Zero
data Nat =
 Zero
 | Succ Nat
 deriving (Eq, Show)

-- Any Natural can be represented by an Integer
natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ s) = 1 + (natToInteger s) 

-- Negative numbers are not valid natural numbers
integerToNat :: Integer -> Maybe Nat 
integerToNat 0 = Just Zero
integerToNat n | n < 0 = Nothing
integerToNat n = Just $ convert n
 where
  convert 0  = Zero
  convert n  = Succ $ convert $ n-1

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- catamorphism 
-- applies the function to the value contained in Maybe and returns the result
-- If we have a value constant of Nothing, return the default value 
-- 0 (+1) (Just 1) = 2
-- 0 (+1) Nothing = 0
applyMaybe :: b -> (a -> b) -> Maybe a -> b
applyMaybe dflt f Nothing = dflt
applyMaybe dflt f (Just val) = f val

fromMaybe :: a -> Maybe a -> a
fromMaybe ident Nothing = ident
fromMaybe _ (Just v) = v

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

maybesToList :: Maybe a -> [a]
maybesToList Nothing = []
maybesToList (Just n ) = n:[]

-- realizeMaybes [Just 1, Nothing, Just 3] = [1,3]
realizeMaybes :: [Maybe a] -> [a]
realizeMaybes [] = []
realizeMaybes ((Just x):xs) = x:realizeMaybes xs
realizeMaybes (Nothing:xs) = realizeMaybes xs

-- flipMaybe [Just 1, Nothing, Just 3] = Just [1,3]
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe ms = Just (realizeMaybes ms)
 where
  realizeMaybes [] = []
  realizeMaybes ((Just v):ms) = v:realizeMaybes ms
  realizeMaybes (Nothing:ms) = realizeMaybes ms

--
-- ==================================================
--

leftsOnly :: [Either a b] -> [a]
leftsOnly [] = []
leftsOnly ((Left x):xs) = x:leftsOnly xs
leftsOnly (_:xs) = leftsOnly xs

rightsOnly :: [Either a b] -> [b]
rightsOnly xs = foldr (\x acc -> 
 case x of 
  (Right x) -> x:acc 
  _         -> acc
 ) [] xs

leftRightPartition :: [Either a b] -> ([a], [b])
leftRightPartition xs = foldr (\x acc ->
 case x of
  (Left x) ->  (x:(fst acc), snd acc) 
  (Right x) ->  (fst acc, x:(snd acc))
  ) ([],[]) xs 

eitherLeftOrRight :: (a -> c) -> (b -> c) -> Either a b -> c
eitherLeftOrRight f _ (Left a)  = f a
eitherLeftOrRight _ f (Right b) = f b

eitherToMaybe :: (b -> c) -> Either a b -> Maybe c
eitherToMaybe f (Left _) = Nothing
eitherToMaybe f (Right v) = Just (f v)

eitherToMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherToMaybe' f (Left l)  =  Nothing  
eitherToMaybe' f (Right r) =  Just (eitherLeftOrRight f  f (Right r) )

--
-- ==============================================
--

myIterate :: (a -> a) -> a -> [a] 
myIterate f v = v : myIterate f (f v)   

--
--       unfoldr :: (s -> Maybe (v, s)) -> s -> [v]
--       unfoldr   (\s -> if s > 10 then Nothing else Just (s,  (s+1)) ) 1
--                   ^                      ^               ^      ^
--                   |                      |               |      |
--              current seed value        done?        current / updating seed value
-- 
-- result: [1,2,3,4,5,6,7,8,9,10]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f v  =  
 case f v of
  Nothing -> []
  Just (v, a) -> v : myUnfoldr f a


myIterate' :: (a -> a) -> a -> [a]
myIterate' f v = myUnfoldr (\a -> Just ( a, f a )) v 


--
-- ==================================================
--

{-toTree :: [a] -> Tree a
toTree (x:y:z:[]) = Node (Leaf x) (y) (Leaf z)
toTree (x:y:[])   = Node (Leaf x) (y) (Empty)
toTree (x:[])     = Node (Empty) (x) (Empty)
toTree []         = Empty
toTree (x:xs)     = let m     = (length xs) `div` 2
                        (l,r) = splitAt m xs
                    in Node (toTree l) x (toTree r)
-}


data BinaryTree a = 
 Leaf
 | Node (BinaryTree a) a (BinaryTree a) 
 deriving (Eq, Ord, Show)


-- usage sample: 
-- balancedTreeUnfold (\e  -> if (e <= 2 ) then Just (e, e+1) else Nothing ) 1
-- Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)
balancedTreeUnfold :: (acc -> Maybe (v, acc)) -> acc -> BinaryTree v
balancedTreeUnfold f v =
 case f v of
  Nothing -> Leaf
  Just (v, acc) ->
   let left = balancedTreeUnfold f acc
       right = balancedTreeUnfold f acc
   in Node left v right


-- 
-- sample usage:
-- treeUnfold  (\e  -> if (e <= 2 ) then Just (e+5, e, e+1) else Nothing ) 1
-- Node Leaf 1 (Node Leaf 2 Leaf)

treeUnfold :: (acc -> Maybe (acc, v, acc)) -> acc -> BinaryTree v
treeUnfold f v = 
 case f v of
  Nothing -> Leaf
  Just ( l, v, r ) -> 
   let left = treeUnfold f l
       right = treeUnfold f r
   in Node left v right

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = balancedTreeUnfold (\v -> if(v <= n) then Just(v, v+1) else Nothing ) 1




