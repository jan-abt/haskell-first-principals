module
    Zed
        where

f :: Maybe Integer 
f = Just 1

g :: Maybe String 
g = Just "1"

h :: Maybe Integer 
h = Just 10191

zed :: a -> b -> c -> (a, b, c) 
zed = (,,)

-- this implementation is not going to work !!!
--
-- doSomething = do 
--     a <- f
--     b <- g
--     c <- h 
--     zed a b c

doSomething =
    f >>= (\f' -> 
        g >>= (\g' -> 
            h >>= (\h' -> 
                return (zed f'  g'  h') 
            )
        )
    )

doSomething' = do 
    a <- f
    b <- g
    c <- h
    return (zed a b c)

