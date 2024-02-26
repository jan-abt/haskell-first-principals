{-# LANGUAGE OverloadedStrings #-}

module
    IntegerDoubleParser
        where

import Text.Trifecta


{-  
    #################################
    ### Text.Trifecta.parseString ###
    #################################

    parseString :: Parser a -> Delta -> String -> Result a
    
  args:  
    Parser a:  the parser we’re going to run against the input
    Delta: a Delta (use mempty to provide the do-nothing input)
    String the input String we’re parsing, 
  result 
    is either the thing we wanted of type "𝑎" 
    or an error string to let us know something went wrong

-}
-- 1 | 1.0
type IntegerOrDouble = Either Integer Double

parseFraction :: Parser IntegerOrDouble
parseFraction = do
    num <- decimal
    _ <- char '/'
    denom <- decimal
    case denom of
            0 -> fail "Denominator cannot be zero"
            _ -> let (wp, dp) = properFraction (fromInteger num / fromInteger denom)
                  in case dp of 
                     0 -> return $ Left wp
                     _ -> return (Right ( fromInteger num  /   fromInteger  denom )  )

main :: IO ()
main = do
    print $ parseString parseFraction mempty "1/0" -- should fail
    print $ parseString parseFraction mempty "5" -- should fail
    print $ parseString parseFraction mempty "0/1"
    print $ parseString parseFraction mempty "1/2"
    print $ parseString parseFraction mempty "1/1"
    print $ parseString parseFraction mempty "3/2"
    print $ parseString parseFraction mempty "2/1"
    





