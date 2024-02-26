{-# LANGUAGE OverloadedStrings #-}

module
    PolymorphicParser
        where
import Data.Attoparsec.Text (parseOnly) 
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta        

{-

While the polymorphic parser combinators in the parsers library 
enable you to write parsers, which can then be run with various parsing libraries, 
this doesn’t free you of understanding the particularities of each.

    Run main and note the different error behaviors! 
    
-}

badFraction :: IsString s => s
badFraction = "1/0" 

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m, MonadFail m) => m Rational 
parseFraction = do
    numerator <- decimal
    _ <- char '/' 
    denominator <- decimal 
    case denominator of
        0 -> fail "Denominator cannot be zero" 
        _ -> return (numerator % denominator)

main :: IO () 
main = do

    putStrLn "-------------------------------------"
    putStrLn "----------- Attoparsec Parser Combinators --------------"
    putStrLn "-------------------------------------"

    -- parseOnly is Attoparsec
    print $ parseOnly parseFraction badFraction 
    print $ parseOnly parseFraction shouldWork 
    print $ parseOnly parseFraction shouldAlsoWork 
    print $ parseOnly parseFraction alsoBad

    putStrLn "------------------------------------"
    putStrLn "------------ Trifecta Parser Combinators  --------------"
    putStrLn "------------------------------------"

    -- parseString is Trifecta
    print $ parseString parseFraction mempty badFraction 
    print $ parseString parseFraction mempty shouldWork 
    print $ parseString parseFraction mempty shouldAlsoWork 
    print $ parseString parseFraction mempty alsoBad
   
    putStrLn "------------------------------------"
    putStrLn "------------ Parsec Parser Combinators  --------------"
    putStrLn "------------------------------------"

    --adding back tracking manually, via "try"    
    -- let p = try (string "ha") <|> string "hi" >> char '1' 
    -- print $ parse p mempty "hi1" 
    -- print $ parseString p mempty "hi1" 
    -- print $ parseOnly p mempty "hi1" 
    
    putStrLn "------------------------------------"
    putStrLn "-------------- Done ----------------"
    putStrLn "------------------------------------"

