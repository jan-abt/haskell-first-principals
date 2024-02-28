module
    BiFunctor
        where 

-- "bf" is the type parameter representing the type constructor (the "type in waiting").
-- It has the kind (* -> * -> *) 
-- How do we figure that?
-- In "bimap" we see "a" and "c", 
-- the two type parameters that "bf" is expected to take to become a concrete type.
-- So, when looking at "BiFunctor bf", we can interpret "bf" as being 
-- a type constructor that takes two type arguments.
class BiFunctor bf 
    where 
        bimap :: (a -> b) -> (c -> d) -> bf a c -> bf b d        
        bimap f g pac = (first f . second g) pac

        first :: (a -> b) -> bf a c -> bf b c
        first f bfac =  bimap f id bfac

        second :: (b -> c) -> bf a b -> bf a c
        second f bfab = bimap id f bfab

-- =============================================================== -- 
  
data Deux a b = 
    Deux a b
    deriving (Show, Eq)

instance BiFunctor Deux
    where
      bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
      bimap f g (Deux a c) = Deux (f a) (g c)

-- bimap  (+5) (+5) (Deux 10 10)
-- first  (+5) (Deux 10 10)
-- second (+5) (Deux 10 10)

-- =============================================================== --    

data Const a b = 
      IgnoreR a
    | IgnoreL b
    deriving (Show, Eq)       

instance BiFunctor Const
    where
        bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
        bimap f _  (IgnoreR a)  = IgnoreR (f a)
        bimap _  f (IgnoreL b) = IgnoreL (f b)

 -- bimap  (+1) (+21) (IgnoreR 9)            
 -- bimap  (+1) (+21) (IgnoreL 9)            

-- =============================================================== --    

-- Constructor "Drei" takes 3 values of types "a", "b" and "c", creating a type with 3 fields.
data Drei a b c = 
    Drei a b c  -- in order to keep in line with the expexted kind (* -> * -> *) for any BiFunctor instance, 
                -- we will have to "bind" one type argument to the structure 
    deriving (Show, Eq)

instance BiFunctor (Drei a') -- "fixing" a' the structure 
    where
        bimap :: (a -> b) -> (c -> d) -> Drei a' a c -> Drei a' b d
        bimap f g (Drei a b c) = Drei a (f b) (g c)
-- bimap  (*2) (*3) (Drei 100 12 12)     

-- =============================================================== --    

data SuperDrei c a b = 
    SuperDrei a b 
    deriving (Show, Eq)

instance BiFunctor (SuperDrei c') 
    where
        bimap :: (a -> b) -> (c -> d) -> SuperDrei c' a c -> SuperDrei c' b d
        bimap f g (SuperDrei a b) = SuperDrei (f a) (g b)
-- bimap  (*2) (*3) (SuperDrei 100 12 12)     

-- =============================================================== --    

-- If you want a BiFunctor instance for SemiDrei, you must fix the first (a) and second type parameter (b).
-- It's not possible to fix the right most.
-- BiFunctor is designed for type constructors of kind * -> * -> *, 
-- Therefor it expects you to fix the parameters that are left-most first, then the next one to the right of it, and so on.    
newtype SemiDrei a b c = 
    SemiDrei c
    deriving (Show, Eq)

instance BiFunctor (SemiDrei c) 
    where
    bimap _ g (SemiDrei c) = SemiDrei (g c)

-- bimap  (*2) (*3) (SemiDrei  12)     

-- =============================================================== --    

data Quadriceps c1 c2 c d = 
    Quadzzz c1 c2 c d    
    deriving (Show, Eq)

instance BiFunctor (Quadriceps c d )  
    where
        bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) ( g d)

-- bimap  (*2) (*3) (Quadzzz 100  100 12 12)    

-- =============================================================== --    

data Either' a b = 
    Left' a | Right' b    
    deriving (Show, Eq)

instance BiFunctor Either' 
    where
      bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
      bimap f _ ( Left'  l) = Left' (f l)
      bimap _ g ( Right' r) = Right' (g r)

--  bimap  (*2) (*3) (Left'  12)  
--  bimap  (*2) (*3) (Right' 12)  
