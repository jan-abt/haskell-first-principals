module 
    Combinators 
        where

import Text.Trifecta
import Control.Applicative ((<|>))

{-
    Parser Combinators
    Parser combinators are functions that combine smaller parsers to create more complex parsers.
    They are commonly used in the field of programming language theory and compiler construction for building parsers.
    Examples include combinators for sequencing, alternation, repetition, and lookahead.

-}    

stop :: Parser a
stop = unexpected "stop"

-- Parser for "1", "12", or "123" using "string" parser
oneTwoThree :: Parser String
oneTwoThree =
    string "123" <|> string "12" <|> string "1"

-- Parser for "1", "12" or "123" using "char" parser
oneTwoThree' :: Parser String
oneTwoThree' =
   let a =  char '1' >>= (\one -> return [one])
       b =  char '1' >>= (\one -> char '2' >>= (\two -> return [one, two]))
       c =  char '1' >>= (\one -> char '2' >>= (\two -> char '3' >>= (\three -> return [one, two, three])))
   in  try c <* eof <|> try b <* eof <|> try a <* eof

-- Parser for "1", "12" or "123" followed by stop
oneTwoThreeStop :: Parser String
oneTwoThreeStop = oneTwoThree *> stop

-- Parser for "1", "12" or "123" followed by eof
oneTwoThreeEOF :: Parser String
oneTwoThreeEOF = oneTwoThree <* eof

testParseString :: Parser String -> IO ()
testParseString p = 
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
    print $ parseString p mempty "123"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "oneTwoThree:"
    testParseString oneTwoThree
    pNL "oneTwoThree':"
    testParseString oneTwoThree'
    pNL "oneTwoThreeStop:"
    testParseString oneTwoThreeStop
    pNL "oneTwoThreeEOF:"
    testParseString oneTwoThreeEOF