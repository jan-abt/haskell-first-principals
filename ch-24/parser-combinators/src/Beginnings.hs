module
        Beginnings
                where


import Text.Trifecta ( CharParsing(char), Parsing(unexpected), Parser )
import Text.Trifecta.Parser (parseString)
import  Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' :: Parser b
one' = one >> stop
-- equivalent to char '1' >> stop    

-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop


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

testTrifectaParseString :: Parser Char -> IO ()
testTrifectaParseString p = print $ parseString p mempty "123456"

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

testEndOfInput :: String -> IO ()
-- The <* operator is used to apply a parser and discard the result of the second parser, 
-- keeping only the result of the first one.
testEndOfInput input =
    let parser = (char '1' <* eof)
        result =  parseString parser mempty input
    in print result 
{-
    Failure (ErrInfo {_errDoc = (interactive):1:2: error: expected: end of input
1 | 123456<EOF> 
  |  ^          , _errDeltas = [Columns 1 1]})    
-}

main :: IO ()
main = do
    pNL "stop:"
    testTrifectaParseString stop
    pNL "one:"
    testTrifectaParseString one
    pNL "one':"
    testTrifectaParseString one'
    pNL "oneTwo:"
    testTrifectaParseString oneTwo
    pNL "oneTwo':"
    testTrifectaParseString oneTwo'