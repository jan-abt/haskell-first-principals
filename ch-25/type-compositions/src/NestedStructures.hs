module 
    NestedStructures 
        where
import Data.Functor.Identity  

{-
 We can construct datatypes that correspond to, or look very much like function composition.
 Note that any polymorphic variables now represent type constructors, not term level variables.
-}

-- ======================================================================== --

-- OneLevelNesting contains a polymorphic type constructor f,
-- which accepts one argument, "arg"
newtype OneLevelNesting f arg =
    OneLevelNesting (f arg) 
    deriving (Eq, Show)

instance Functor f => Functor (OneLevelNesting f) 
    where
        fmap :: Functor f => (a -> b) -> OneLevelNesting f a -> OneLevelNesting f b
        fmap f (OneLevelNesting fa) = OneLevelNesting $ fmap f fa

-- ======================================================================== --

-- again, f (g arg) is the type! f and g are type constructors    
newtype TwoLevelNesting f g arg = 
    TwoLevelNesting { getTwoLevelNesting :: f (g arg) } 
    deriving (Show, Eq)

instance  (Functor f, Functor g) => Functor (TwoLevelNesting f g )
     where 
        fmap :: (a -> b) -> TwoLevelNesting f g a -> TwoLevelNesting f g b
                                                            --       ( fmap . fmap) (*5)  [Just 3] 
        fmap f  (TwoLevelNesting fgarg ) = TwoLevelNesting $ ( fmap . fmap)   f    fgarg
                                                            -- OR:     fmap  (fmap  (*5)) [Just 3]
{-  (.) ::
         (b -> c) -> (a -> b) -> a -> c
    fmap ::
         ( a        ->  b  )   -> f a     -> f b
    fmap . fmap ::
         ((a -> b)  ->  f a)   -> f(f a)  -> f(f b)
-}        
            
-- ======================================================================== --

newtype ThreeLevelNesting f g h arg = 
    ThreeLevelNesting (f (g (h arg))) 
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (ThreeLevelNesting f g h) 
    where
        fmap :: (a -> b) -> ThreeLevelNesting f g h a -> ThreeLevelNesting f g h b
        fmap f (ThreeLevelNesting fgha) = ThreeLevelNesting $ (fmap . fmap . fmap) f fgha    


-- ============== Examples of other nested structures using standard types ======================= --

inner :: Maybe (Identity (a -> b)) -> Maybe (Identity a -> Identity b)
inner = fmap (<*>)

outer :: Maybe (Identity a -> Identity b) -> Maybe (Identity a) -> Maybe (Identity b)
outer = (<*>)  

-- ghci> import Data.Functor.Identity      
-- ghci> f = Just (Identity (*2))
-- ghci> v = Just (Identity 5)
-- ghci> outer (inner f )  v

inner' :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
inner' = (fmap . fmap) (<*>)

middle' :: [Maybe (Identity a -> Identity b)] -> [Maybe (Identity a) -> Maybe (Identity b)]
middle' = fmap (<*>)

outer' :: [Maybe (Identity a) -> Maybe (Identity b)] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
outer' = (<*>)        

-- ghci> import Data.Functor.Identity      
-- ghci> f' = [Just (Identity (*2))]
-- ghci> v' = [(Just (Identity 5))]
-- ghci> outer' ( middle' (inner' f' ) ) v'      


main :: IO () 
main = do
   putStrLn "-- ========================== Examples of types that are composed of nested structures ============================= --\n"
   
   let o::OneLevelNesting Maybe Int = OneLevelNesting (Just 1)
   putStr "\t "
   print  o 

   let tw:: TwoLevelNesting [ ] Maybe Double = TwoLevelNesting [Just 1.0, Just 2.5]
   putStr "\t "
   print tw

   let tr::ThreeLevelNesting [] Maybe Maybe Int = ThreeLevelNesting [Just (Just 1)]
   putStr "\t "
   print  tr

   putStrLn "\n-- =================== Applying functions to types that are composed of nested structures ====================== --\n"
   
   putStr "\t(*5) <$> OneLevelNesting (Just 2) = "
   print $ (*5) <$> OneLevelNesting (Just 2)
   putStr "\t(*5) <$> TwoLevelNesting (Just (Just 2)) = "
   print $ (*5) <$> TwoLevelNesting (Just (Just 2))
   putStr "\t(*5) <$> ThreeLevelNesting (Just (Just (Just 2))) = "
   print $ (*5) <$> ThreeLevelNesting (Just (Just (Just 2)))

   putStrLn "\n-- ========================== Examples of other nested structures using standard types ============================= --\n"
   
   let f = Just (Identity (*2))
   let v = Just (Identity 5)
   putStr "\tTwo levels of nesting: "
   print $ outer (inner f )  v

   let f' = [Just (Identity (*2))]
   let v' = [Just (Identity 5)]
   putStr "\tThree levels of nesting: "
   print $ outer' ( middle' (inner' f' ) ) v'      
