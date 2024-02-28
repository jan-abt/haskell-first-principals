module
    List
        where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers       

data List a = 
    Nil
    | Cons a (List a) 
    deriving (Eq, Show)

instance Functor List 
    where 
        fmap _ Nil = Nil
        fmap f (Cons a ls) =  Cons (f a) (fmap f ls)
 
instance Applicative List
    where  
        pure x = Cons x (Nil) 
        fs <*> vs = fs `outerApply` vs

outerApply :: List (a -> b) -> List a -> List b
outerApply Nil _ = Nil
outerApply _ Nil = Nil
outerApply (Cons f fs) vs = 
    innerApply fs vs
        where 
            -- When the current function has been applied to all values,
            -- start over using the next function in the list
            innerApply fs Nil = fs `outerApply` vs
            -- for the last function, apply each value using fmap
            innerApply Nil vs' = (fmap f vs') 
            -- using recursive function application, create a nesting of partially applied functions.    
            -- the final value, Nil, which is needed to fully apply the grouping and thus turn it into a proper List structure,
            -- will be provided in last step of fmap, see: "fmap _ Nil = Nil"
            -- (Cons 2 $ Cons 2 $ ...) Nil
            innerApply fs (Cons v vs') = Cons (f v) $ (innerApply fs vs') 


-- FOR QUICK CHECK DATA GENERATION    
-- generates random variations of List, in the Gen context such as, 
-- for example:  Gen (Cons 1 (Cons 2 (Cons 3 Nil)))
instance Arbitrary a => Arbitrary (List a) 
    where        -- controls the sizes of generated lists
        arbitrary = sized buildList
            where
                buildList 0 = pure Nil
                buildList n = frequency -- finetune data generation! Let the generated list be Nil only a 5th of the time.
                    [ 
                        (1, pure Nil),
                        -- 1. use <$> to lift "Cons" into "arbitrary", which is our "Gen a" context  
                        -- Gen Cons n                      
                        -- 2. "buildList" appends more arbitray Cons n or Nil, each inside a Gen context.
                        -- Gen <function> ... | Gen Nil
                        --
                        -- The line of code below ultimately generates variations of terms such as this one:  
                        --   Cons <$> Gen Cons 1 <*> Cons 2 <*> Cons 3 <*> Gen Nil 
                        -- In order to apply our functions at runtime, we need to lift them out of their individual contexts into a single one. 
                        -- Therfore the <*>'s .
                        -- At runtime the above given example would resolve to the final construct: 
                        --   Gen (Cons 1 (Cons 2 (Cons 3 Nil)))
                        (5, Cons <$> arbitrary <*> (buildList (n - 1)))
                    ]
        
instance Eq a => EqProp (List a) 
    where 
        (=-=) = eq 

-- sample usage:    
-- quickBatch $ applicative (Cons (1, 2, 3) Nil)
-- let functions = Cons (+1) (Cons (*2) (Cons (negate) Nil) )
-- let values = Cons 1 (Cons 2 Nil)
-- functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 (Cons (-1) (Cons (-2) Nil)))))

