{-# LANGUAGE QuasiQuotes #-}

module
    IntegerStringParser
        where

import Control.Applicative    
import Text.Trifecta 
import Text.RawString.QQ

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

type IntegerOrString =
   Either Integer String

eitherOr :: String 
eitherOr = [r|123
abc
456
def|]    

ignoreSpaces :: CharParsing m => m ()
ignoreSpaces = skipMany (char ' ')

parseNumbersOrStrings :: Parser IntegerOrString
parseNumbersOrStrings = 
                 -- combining 3 parsers in order to get to parse a number
     try (Left <$> (ignoreSpaces >> integer <* ignoreSpaces))
    <|>          -- combining 3 parsers in order to get to parse text
     try (Right <$> (ignoreSpaces >> some letter <* ignoreSpaces) )

main :: IO ()
main = do    
    print $ parseString (many parseNumbersOrStrings) mempty "only Text "
    print $ parseString (many parseNumbersOrStrings) mempty " 123 456"
    print $ parseString (many parseNumbersOrStrings) mempty " 123 Some Text 789 And More Text"
    print $ parseString (many parseNumbersOrStrings) mempty "123 Numbers And Text 789 "
    print $ parseString (many parseNumbersOrStrings) mempty eitherOr

    -- print multiline

    -- print eitherOr

    