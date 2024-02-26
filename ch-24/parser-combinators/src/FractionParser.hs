{-# LANGUAGE OverloadedStrings #-}

module
    FractionParser
        where

import Data.Ratio ((%)) 
import Text.Trifecta        


{-  
    #################################
    ### Text.Trifecta.parseString ###
    #################################

    parseString :: Parser a -> Delta -> String -> Result a
    
  args:  
    Parser a:  the parser we’re going to run against the input
    Delta: a Delta (use "mempty" to provide the do-nothing input)
    String the input String we’re parsing, 
  result 
    is either the thing we wanted of type "𝑎" wrapped in "Success",
    or an Error  to let us know something went wrong

-}


-- TEST DATA
divideByZero :: String
divideByZero = "1/0"  

validFraction :: String
validFraction = "1/2"    

validFraction2 :: String
validFraction2 = "2/1"       

incompleteFraction :: String
incompleteFraction = "10"    

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    -- if an exception is raised (division by zero), this would ends our program
    return (numerator % denominator)

parseFraction' :: Parser Rational
parseFraction' = 
    decimal >>= (\numerator ->
       char '/' >> 
            decimal >>= (\denominator -> 
                -- if an exception is raised (division by zero), this would ends our program
                return (numerator % denominator) )
    )


parseFractiongracefully :: Parser Rational
parseFractiongracefully = do
    numerator <- decimal 
    _ <- char '/'
    denominator <- decimal 
    case 
        denominator of
            0 -> fail "Denominator cannot be zero" 
            _ -> return (numerator % denominator)    

parseFractiongracefully' :: Parser Rational
parseFractiongracefully' = 
    decimal >>= (\numerator ->
       char '/' >> 
            decimal >>= (\denominator -> 
                case denominator of
                    0 -> fail "Denominator cannot be zero" 
                    _ -> return (numerator % denominator) 
            )
    )    

main :: IO () 
main = do
    print $ parseString parseFractiongracefully' mempty validFraction 
    print $ parseString parseFractiongracefully' mempty validFraction2 
    print $ parseString parseFractiongracefully' mempty divideByZero 
    {-
     if not handled, "divideByZero" will raise an exception that will halt the program.
     Catching exceptions is okay, 
     but with some classs of exceptions something may be fundamentlly wrong with the program
     and so, aborting may be the right thing to do
    -}
    print $ parseString parseFractiongracefully' mempty incompleteFraction    
    
    -- disregards the value of the left monad and render the value of the right monad, eof :: Parsing m => m (), a.k.a "unit"
    print $ parseString (integer >> eof) mempty "123"
    -- disregards the value on the right monad and render the value of the left monad integer :: TokenParsing m => m Integer, a.k.a "123"
    print $ parseString (integer <* eof) mempty "123"

    print $ parseString (integer <* notFollowedBy (char '-')) mempty "123A-"
    
    print $ parseString (integer <* notFollowedBy (char '-')) mempty "123A"




