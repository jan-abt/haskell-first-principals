module
    Eval
        where

data Eval a b =
    Err a
    |  Val b
    deriving (Eq, Ord, Show)

instance Functor (Eval a)
    where
        fmap _ (Err a) = Err a
        fmap f (Val b ) = Val (f b)
-- fmap (\n -> 
--  case (mod n 2 == 0) of 
--      True -> Val n
--      _ -> Err ( "error on odd value: " ++ (show n))) 
--  [1..10]        

instance Applicative (Eval a)
    where
        pure a = Val a                        
        (<*>) (Err e) _ = Err e
        (<*>) (Val f) v = fmap f v
-- pure 2 :: (Eval String Integer)     
-- pure (* 2) <*> Val 5   
-- Err "init failed" <*> Val 5
-- pure (* 2) <*> Err "applicative failed"

instance (Semigroup b) => Semigroup (Eval a b)
    where   
        (<>) (Val v) (Val v') = Val (v <> v')
        (<>) e _ =  e

instance (Semigroup b, Monoid b) => Monoid (Eval a b)
    where
        mempty =  Val mempty 
        mappend = (<>)

instance Foldable (Eval a)
    where 
        foldMap _ (Err _) = mempty -- short ciruited monoid m
        foldMap f (Val v) = f v
-- foldMap (\n -> Val ("Hi, " ++ n)) (Val "Jan" )     
-- foldMap (\n -> Val (Sum (n + 1))) (Just 5 )
        foldr _ a (Err _) = a
        foldr f a (Val v) = f v a
-- foldr (\n a -> Val("Hi, " ++ n)) (Val "") (Val "Jan")        
-- foldr (\n a -> Val("Hi, " ++ n)) (Val "") (Err "Oh")

        null             = isErr
            where 
                isErr :: Eval a b -> Bool
                isErr (Err  _) = True
                isErr (Val _) = False    

        length (Err _)  = 0
        length (Val _) = 1                

instance Traversable (Eval a)
    where
        traverse _ (Err e) = pure (Err e)
     -- traverse (a -> f b) ->   t a   ->  f (t b)
        traverse     f         (Val a) = fmap Val (f a)
     -- traverse id $  Val (Just 2)
     -- traverse id $  Val (Right 2)
     -- traverse (\n ->  fmap (*5) n) $  Val [1]
     -- traverse (\n ->  fmap (*5) n) $ Err "e"
     