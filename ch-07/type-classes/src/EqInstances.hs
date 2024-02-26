
module EqInstances  where

data AnInt = AnInt Int

instance Eq AnInt where
 (==) (AnInt i) (AnInt i') = i == i'

data TwoInts = TwoInts Integer Integer

instance Eq TwoInts where
 (==) (TwoInts i j) (TwoInts i' j') = i==i' && j==j'

data StringOrInt = MyInt Int | MyString String

instance Eq StringOrInt where
 (==) (MyInt i) (MyInt i')       = i == i'
 (==) (MyString s) (MyString s') = s == s'
 (==) _ _                        = False


data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
 (==) (Pair a b) (Pair a' b') = (a,b) == (a',b') -- defer equality check to Tuple2


data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
 (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = This a | That a

instance Eq a => Eq (Which a) where  
 (==) (This t) (This t')  = t == t'
 (==) (That t) (That t')  = t == t'
 (==) (This t) (That t')  = t == t'
 (==) (That t) (This t')  = t == t'


data EitherOr a b = Hello a | Goodbye b

instance (Eq x, Eq y) => Eq (EitherOr x y) where
 (==) (Hello a) (Hello a') =  a == a'
 (==) (Goodbye b) (Goodbye b') =  b == b'
 (==) _            _           = False
