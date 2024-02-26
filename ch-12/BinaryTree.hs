module BinaryTree where

data BinaryTree a = 
  Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

expected = Node (Node Leaf 20 Leaf) 10 (Node Leaf 30 Leaf)

actual :: BinaryTree Integer
actual = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)

mapTreeTest =
 if mapTree (*10) expected == actual 
 then print "yup okay!"
 else error "test failed!"

toList :: BinaryTree a -> [a]
toList bn = recurse bn []
 where recurse Leaf acc = acc
       recurse (Node left a right) acc =  (recurse left acc) ++ (a:acc) ++ (recurse right acc)

toAscendingList :: (Ord a) => BinaryTree a -> [a]
toAscendingList bn = recurse bn []
 where
       recurse Leaf acc = acc
       recurse (Node left a right) acc = 
        let result = (recurse left acc) ++ (a:acc) ++ (recurse right acc)                                  
        in sorted result

       sorted [] = []
       sorted (x:xs) = 
        let smaller = [s| s <- xs, s <= x]
            larger  = [l| l <- xs, l > x ]
        in  (sorted smaller) ++ [x] ++ (sorted larger)
          
insert :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert newV Leaf = Node Leaf newV Leaf
insert newV (Node left otherV right) 
                 | newV <  otherV = Node (insert newV left ) otherV right 
                 | newV >= otherV = Node left otherV (insert newV right)
  
foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f acc Leaf = acc
foldrTree f acc (Node lt v rt) =
   let accl = (foldrTree f acc lt )
       acclr = (foldrTree f accl rt )
   in f v acclr

foldrTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree' f a t = foldr f a $ toList t

mapOverList :: (a -> b) ->  [a] -> [b]
mapOverList  _ [] = []
mapOverList f (x:xs) = (f x) : mapOverList f xs

mapOverListWithFoldr :: (a -> b) ->  [a] -> [b]
mapOverListWithFoldr f xs = foldr (\e a -> f e : a) [] xs

mapOverTreeWithFoldr :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b 
mapOverTreeWithFoldr f bt = foldrTree  (\e t -> insert (f e) t) Leaf bt


