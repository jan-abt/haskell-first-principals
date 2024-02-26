{-# LANGUAGE OverloadedStrings #-}


module
    SemanticVersionParser
        where

import Control.Applicative
import Text.Trifecta

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [AlphaNumeric]
type Metadata = [AlphaNumeric]
-- cannot sort numbers like strings. 
data AlphaNumeric = ANS String | ANI Integer deriving (Show)

instance Eq AlphaNumeric
    where
       (==) :: AlphaNumeric -> AlphaNumeric -> Bool
       (==) (ANS s) (ANS s')=  (==) s s'
       (==) (ANI i) (ANI i')=  (==) i i'
       (==) _ _  = False

--  let v1 = parseString parseSemVer mempty "2.1.1"
--  let v2 = parseString parseSemVer mempty "2.1.2"
--  (<) <$> v1 <*> v2       

instance Ord AlphaNumeric
    where
       compare :: AlphaNumeric -> AlphaNumeric -> Ordering
       compare (ANS s) (ANS s') | (==) s s' = EQ
                                | (>) s s' = GT
                                | otherwise = LT
       compare (ANI i) (ANI i') | (==) i i' = EQ
                                | (>) i i' = GT
                                | otherwise = LT
       compare _ _ = EQ
--  let v1 = parseString parseSemVer mempty "2.1.1"
--  let v2 = parseString parseSemVer mempty "2.1.2"       
--  (compare) <$> v1 <*> v2

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq, Ord)

parseSemVer :: Parser SemVer
parseSemVer = do
    maj' <- integer
    _ <- char '.'

    min' <- integer
    _ <- char '.'

    pat' <- integer
    _ <- skipMany $ char '-' <|> char '+'

    rel' <- many ( ( (ANI <$> integer) <|> (ANS <$> some letter)) <* skipOptional (char '.') )

    return (SemVer maj' min' pat' rel' [])


main :: IO ()
main = do
    print $ parseString parseSemVer mempty "2.1.1"
    print $ parseString parseSemVer mempty "1.0.0-U.7.SA.72"
    print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

    
