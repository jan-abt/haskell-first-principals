module IgnoringPossibilities where 

import GHC.Arr    

main :: IO () 
main = do
    putStr "IgnoringPossibilities"

data Possibly a = 
    Impossible
    | Definitely a 
    deriving (Eq, Show)

instance Functor Possibly 
    where 
        fmap f Impossible = Impossible
        fmap f (Definitely a) = Definitely (f a)

--
-- ========================================================================
--

 -- `lifted' because they've been lifted over a functorial structure "ƒ" 
plusOpLifted :: (Functor f, Num b) => f b -> f b 
plusOpLifted = fmap (+1)

showPlusOpLifted :: (Functor f, Show a) => f a -> f String 
showPlusOpLifted = fmap show

--
-- ========================================================================
--

-- The Sum type constructor's kind is * -> * -> *
-- We have to change it to * -> * 
-- We can do this by "fixing" the left most element to the functorial structure
data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)


instance Functor (Sum a) -- "fixing" the left most element to the functorial structure
    where  
        -- even if we tried to, the compiler would not let us apply "f" to the "fixed" element "a".
        -- error: ... ‘a1’ is a rigid type variable bound by the type signature for: ...
        -- however, we could turn our polimorphic "First a" data constructor into a literal one, as in
        -- "First Int", and then we could manipulate "a": fmap f (First a ) = First (a+1) 
        -- this would compile but it would break the functor laws, so don't do this.
        fmap _ (First a ) = First a -- part of the structure
        fmap f (Second b ) = Second (f b) -- free to map over

-- fmap (*2) $ First 5
-- fmap (*2) $ Second 5

-- flip the "fixed" element 
newtype Flipped s a b = Flipped (s b a) deriving (Eq, Show)

instance Functor (Flipped Sum(el)  )
    where 
        fmap f (Flipped (First a)) = Flipped $ First (f a)  -- free to map over
        fmap _ (Flipped (Second b)) = Flipped $ Second b -- part of the structure

-- fmap (*2) $ Flipped (First 5)   
-- fmap (*2) $ Flipped (Second 5)   

--
-- ========================================================================
--

-- The type parameter 𝑏 is a phantom type. It has no corresponding witness at the value/term level.
-- Yet, this data constructor's kind is still  * -> * -> * .
-- In this form is is not suited to make a Functor instance with, 
-- unless we bind the innermost element to the functorial structure
data Constant a b =
    Constant { getConstant :: a } 
    deriving (Eq, Show)

instance Functor (Constant a) 
    where 
        -- we're not allowed to apply the function to a, since it is part of the functorial structure
        -- and the phantom type variable 𝑏 has no value or term-level witness in the datatype.
        fmap _ (Constant b) = Constant b   

-- Despite seeming pointless, Constant is a lawful Functor.

-- usage:
-- getConstant $ fmap (*2) (Constant 3)
-- getConstant $ fmap (const "blah") (Constant 3)

--
-- ========================================================================
--
data WrappedFunctor ft a =
 -- example: WrappedFunctor(Just 2) 
    WrappedFunctor (ft a)
    deriving (Eq, Show)

instance Functor ft => Functor (WrappedFunctor ft)
    where
        fmap f ( WrappedFunctor ft) = WrappedFunctor(fmap f ft)
    
--  usage example:
--  fmap (+2) $ WrappedFunctor(Just 2) 

--
-- ========================================================================
--

fmapMeToo :: IO String
fmapMeToo = fmap (++ " and me too!") getLine    

doMeToo :: IO String
doMeToo = do
    input <- getLine
    return (input ++ " and me too!")

--
-- ========================================================================
--

-- Determine if a valid Functor can be written for the datatype provided.

-- 1. 
-- not a functor ... 
data Boolean =  False | True

-- ... needs wrapper 
data Wrapper a = 
    Wrapper a
    deriving (Eq, Show)
    
instance Functor Wrapper 
    where
        fmap f (Wrapper b) = Wrapper (f b)
 
 -- 2.
data BoolAndMore a = Falsehood a | Truth a

instance Functor BoolAndMore 
    where
        fmap f (Truth e) = Truth ( f e)    
        fmap f ( Falsehood e) =  Falsehood ( f e)    

-- 3.
data BoolAndPossiblyMore a = Truezie a | Falsey

instance Functor BoolAndPossiblyMore 
    where
        fmap f ( Truezie e) =  Truezie ( f e)    
        fmap _ _  =  Falsey        
-- 4.
-- newtype Mu f = InF { outF :: f (Mu f) }  

-- instance Functor (f Mu) 
--     where
--         fmap f fmua  = (fmap f fmua)
-- 5.

-- Define the D type
data D = D (Array Word Word) Int Int deriving (Eq, Show)
 
data Wrapped a = 
    Wrapped D a
    deriving (Eq, Show)

-- Functor instance for D
instance  Functor (Wrapped ) where
   fmap f (Wrapped (D m a x) n) = Wrapped (D m a x) (f n)
 
--
-- ========================================================================
--

-- 1. 
-- Rearrange the arguments to the type constructor of the datatype
--  so that the Functor instance works.

data Tuple a b = Tuple a b  deriving (Eq, Show)

instance Functor (Tuple a) where
    fmap f (Tuple a b) = Tuple a (f b)

-- usage
-- fmap (++", blah...") (Tuple 5 "blah")

newtype Flip t a b = Flip (t b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- usage
-- fmap (*2) (Flip (Tuple 5 "blah"))

data Tuple' a b = Tuple' b a deriving (Eq, Show)

instance Functor (Tuple' b) where
    fmap f (Tuple' a b) = Tuple' (f a) b

-- fmap (*2) (Tuple' 5 "blah")

--
-- ========================================================================
--

data Company a b c =
    Something b
    | DeepBlue a c
    deriving (Eq, Show)

instance Functor (Company e e') 
    where 
        fmap _ (Something b) =  Something b
        fmap f (DeepBlue a c) = DeepBlue a (f c)

-- fmap (*2) (Something 5 )    
-- fmap (*2) (DeepBlue 5 10 )          

newtype Rearranged t a b c = Rearranged (t a c b) deriving (Eq, Show)

instance Functor (Rearranged Company x x') 
    where 
        fmap f (Rearranged (Something b)) = Rearranged $ Something (f b) 
        fmap _ (Rearranged (DeepBlue a c)) = Rearranged $ DeepBlue a c    

-- fmap (*2) $ Rearranged (Something 5 )        
-- fmap (*2) $ Rearranged (DeepBlue 5 10 )

data Company' a c b =
    Something' b
    | DeepBlue' a c
    deriving (Eq, Show)

instance Functor (Company' e e') 
    where 
         fmap f (Something' b) = Something' (f b) 
         fmap _ (DeepBlue' a c) = DeepBlue' a c    

-- fmap (*2) (Something' 5 )        
-- fmap (*2) (DeepBlue' 5 10 )         

--
-- ========================================================================
--

data More a b = 
    L a b a
  | R b a b
 deriving (Eq, Show)

instance Functor (More a) 
    where
        fmap f (L a b a') = L a (f b) a' 
        fmap f (R b a b') = R (f b) a (f b')    

-- fmap (*10) $ L 1 5 1
-- fmap (*10) $ R 1 5 1

newtype Inverted t a b = Inverted (t b a) deriving (Eq, Show)

instance Functor (Inverted More b) 
    where
        fmap f (Inverted(L a b a')) = Inverted $ L (f a) b (f a') 
        fmap f (Inverted(R b a b')) = Inverted $ R b (f a) b'    

-- fmap (*10) $ Inverted (L 1 5 1)
-- fmap (*10) $ Inverted (R 1 5 1)

data More' b a = 
    L' a b a
  | R' b a b
 deriving (Eq, Show)

instance Functor (More' a) 
    where
        fmap f (L' a b a') =  L' (f a) b (f a') 
        fmap f (R' b a b') =  R' b (f a) b'         

-- fmap (*10) (L' 1 5 1)
-- fmap (*10) (R' 1 5 1)

--
-- ========================================================================
--

data Quant a b = 
    Finance
    | Desk a 
    | Floor b    
    deriving (Eq, Show)

instance Functor (Quant a)
     where
        fmap _ Finance = Finance
        fmap _ (Desk a) = Desk a
        fmap f (Floor b) = Floor (f b)

data K a b = 
    K a 
    deriving (Eq, Show)

instance Functor (K a)
     where
        fmap _ (K a) = K a

-- fmap (*2) (K 3)

newtype Flipper f a b = 
    Flipper (f b a) 
    deriving (Eq, Show)   

instance Functor (Flipper K el)
     where
        fmap f (Flipper(K a)) = Flipper $ K (f a)                 

--  fmap (*2) $ Flipper (K 3)       

--
-- ========================================================================
--

data EvilGoateeConst a b = -- phantom type parameter
    GoatyConst a 
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a)
    where
        fmap _ (GoatyConst a) = GoatyConst a

--  fmap (*2) $ GoatyConst 10       

--
-- ========================================================================
--    

-- concrete data example: 
-- LiftItOut Maybe Int = 
--      LiftItOut (Just 2)
data LiftItOut f v = 
    LiftItOut (f v)
    deriving (Eq, Show)

instance (Functor ft) => Functor (LiftItOut ft)    
    where
        fmap ab (LiftItOut fa) = let fb = fmap ab fa
                                 in LiftItOut fb

--  fmap (+3) $ LiftItOut (Just 32)            

--
-- ========================================================================
--

-- concrete data example: 
-- ApplyBoth Maybe Either Int = 
--  ApplyBoth (Just 2) (Right 3)
data ApplyBoth f g a = 
    ApplyBoth (f a) (g a)
    deriving (Eq, Show)

instance (Functor fa, Functor fa') => Functor (ApplyBoth fa fa')
    where
        fmap ab (ApplyBoth fa fa') = let fb  = (fmap ab fa)
                                         fb' = (fmap ab fa')
                                    in (ApplyBoth fb fb')

--  fmap (+3) $ ApplyBoth (Just 2) (Right 3)

--
-- ========================================================================
--

-- ApplyOne Maye Int Either String = 
--  ApplyOne (Just 1) (Right "Jan")
data ApplyOne f g a  = 
     ApplyOne f (g a) 
     deriving (Eq, Show)

instance  (Functor fa') =>  Functor (ApplyOne fa fa')
    where
        fmap ab (ApplyOne fa fa')  = let fb' = (fmap ab fa' )
                                       in (ApplyOne fa fb')

-- fmap (+2) $ ApplyOne (Right "Jan") (Just 2)                
-- fmap (++"..") $ ApplyOne (Just 1) (Right "Jan") 

--
-- ========================================================================
--
-- * -> * -> *
data Select a b = 
             --  str   a      b  
     LS b a  --  LS    42  "Right"  (fixing/binding "b" to the structure/"LS")  fmapping over the other
   | RS a b  --  RS    42  "Right"  (fixing/binding "a" to the structure/"RS")  fmapping over the other
   deriving (Eq, Show)

--  * -> *     
instance Functor (Select a) -- have to bind one of the type parameters to the structure to establish the appropriate "kind"
    where
        fmap f (LS a b) = LS (f a) b 
        fmap f (RS a b) = RS a (f b) 
-- fmap (+42) $ LS 42 "Right"
-- LS 84 "Right"
-- fmap (++"..") $ RS 42 "Right"
-- RS 42 "Right.."

data Select' f g a b =                
     LS' (f b) (g a)  --   LS' (Just 42) (Right "Nice")       
   | RS' (g a) (f b)  --   RS' (Just 42) (Right "Nice")
   deriving (Eq, Show)

instance (Functor f) => Functor (Select' f g a)
    where
        fmap f (LS' fa fa') = LS' (fmap f fa) fa' 
        fmap f (RS' fa fa') = RS' fa (fmap f fa')
        
-- fmap (+42)     $ LS' (Just 42) (Right "Nice")        
-- fmap (++"...") $ RS' (Just 42) (Right "Nice")

--
-- ========================================================================
--

-- Notorious (Just 2) (Just "S") (Just 'C')    
data Notorious g o a t = 
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor go ) => Functor (Notorious go ga g)
    where
        fmap f ( Notorious go ga g) =  (Notorious go ga (fmap f g))

-- fmap (:[]) $ Notorious (Just 2) (Just "S") (Just 'C')    

--
-- ========================================================================
--             

-- C 3 (C 2 (C 1 Nil))
data List a = 
    Nil
  | C a (List a)    
  deriving (Eq, Show)

instance Functor List
    where
        fmap f Nil = Nil
        fmap f (C a ls) = C (f a) (fmap f ls)

-- fmap (*5) $ C 3 (C 2 (C 1 Nil))

--
-- ========================================================================
--

-- MoreGoats (OneGoat 1) NoGoat (MoreGoats (OneGoat 5) NoGoat  (OneGoat 5))
data GoatLord a = 
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)

instance Functor GoatLord
    where
        fmap f NoGoat = NoGoat
        fmap f (OneGoat a) = OneGoat (f a)
        fmap f (MoreGoats  a  a'  a'') = 
            MoreGoats (fmap f a) (fmap f a') (fmap f a'')

-- fmap (+5) $ MoreGoats (OneGoat 1) NoGoat (MoreGoats (OneGoat 5) NoGoat  (OneGoat 5))

--
-- ========================================================================
--   
data TalkToMe a = 
    Halt
    | Print String a
    | Read (String -> a)
    deriving ( Show)

instance  Show (String -> a)
    where
        show s = "(String -> a)"

-- fmap (const 4) $ Halt  
-- fmap (+4) $ Print "Jan" 4
-- fmap length (Read (\s -> s ++ " World!"))
instance Functor TalkToMe
    where 
        fmap _ Halt = Halt
        fmap f (Print s a) = Print s (f a)   
        fmap f (Read f') = Read ( f <$> f' )   
