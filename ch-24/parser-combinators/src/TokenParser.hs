{-# LANGUAGE OverloadedStrings #-}

module
    TokenParser
        where

import Control.Applicative
import Test.Hspec ( shouldBe, hspec, describe, it )

-- parsers 0.12.3, trifecta 1.5.2 
import Text.Trifecta


-- ============================================================ --
-- ============================================================ --
-- ============================================================ --

{-

    "For any type m to be an instance of TokenParsing, 
     it must also be an instance of CharParsing."

    class CharParsing m => TokenParsing m where
    
    ghci> :t token
    token :: TokenParsing m => m a -> m a

    ghci> :t some
    some :: Alternative f => f a -> f [a]

                       parser delta  string
    ghci> parseString (digit) mempty "12\n6"
        Success '1'
    ghci> parseString ( some $ digit ) mempty "12\n6"
        Success "12"
    ghci> parseString ( some $ token digit ) mempty "12\n6"
        Success "126"    
    ghci> parseString ( some $ some $ digit ) mempty "12\n6"
        Success ["12"]
    ghci> parseString ( some $ some $ token digit ) mempty "12\n6"
        Success ["126"]        
    ghci> parseString ( some $ token $ some $ digit ) mempty "12\n6"
        Success ["12","6"]

-}


parseIntegerString :: Parser [Integer]
parseIntegerString = some $ do
    i <- token digit
    return (read [i])

main :: IO ()
main = do
    -- 1 --
    let d = "12\r\t\n6"

    let p = some $ token digit
    let l = parseString p mempty d
    putStrLn ("\n" ++ show l)

    let p' = some $ some $ token digit
    let l' = parseString p' mempty d
    putStrLn (show l')

    let p'' = some $ token $ some digit
    let l'' = parseString p'' mempty d
    putStrLn (show l'')

    let l''' = parseString (length <$> p) mempty d
    putStrLn (show l''')


    -- 2 --
    print $ parseString (some $ token anyChar) mempty "12\r\t[\n6"    
    print $ parseString (some $ token (noneOf "[")) mempty "12\r\t[\n6"
    -- 3 --
    hspec $ do
        describe "Simple Integer Tokenizing Parser" $
            it "can tokenize a digit" $ do
                let r = parseString  parseIntegerString mempty "123"
                print r
                convertResultToMaybe r `shouldBe` Just [1,2,3]


convertResultToMaybe :: Result a -> Maybe a
convertResultToMaybe (Success a) = Just a
convertResultToMaybe _ = Nothing

twoOrMoreDigitParser :: (Monad m, CharParsing m) => m [Char]
-- consume the first character via "digit"
-- In the next operation, bound to the same monadic context, 
-- consume the next character, once again via "digit".
-- Then, stick the result of the 2nd operation into a list, via "some".
-- and finally add the first one to that list as well
twoOrMoreDigitParser = 
    digit >>= \dig -> fmap (dig :) (some digit)
    
