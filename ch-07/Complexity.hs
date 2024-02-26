data Complexity =
 Trivial | Complex

instance Eq Complexity where
   (==) Trivial Trivial = True
   (==) Complex Complex = True
   (==)  _ _            = False


