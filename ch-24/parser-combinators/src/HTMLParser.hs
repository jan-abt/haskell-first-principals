{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module
    HTMLParser
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
    Delta: a Delta (use "mempty" to provide the do-nothing input)
    String the input String we’re parsing, 
  result 
    is either the thing we wanted of type "𝑎" wrapped in "Success",
    or an Error  to let us know something went wrong

-}


inputHTML :: String
inputHTML = [r|<HTML>
    <HEAD>
        <TITLE>Auto-generated html formated source</TITLE>
        <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
    </HEAD>
    <BODY LINK="#0000ff" VLINK="#800080" BGCOLOR="#ffffff">
        <P> </P>
    </BODY>
|]


lt :: CharParsing m => m String
lt = string "<"

gt :: CharParsing m => m String
gt = string ">"

lr :: CharParsing m => m String
lr = (:[]) <$> letter

sp :: CharParsing m => m String
sp = string " "

nl :: CharParsing m => m String
nl = string "\n"

tb :: CharParsing m => m String
tb = string "\t"

hy :: CharParsing m => m String
hy = string "-"

fw :: CharParsing m => m String
fw = string "/"

el :: CharParsing m => m String
el = string "="

co :: CharParsing m => m String
co = string ";"

pd :: CharParsing m => m String
pd = string "#"

qu :: CharParsing m => m String
qu = string "\""

fromString :: CharParsing f => f String
fromString = lt <|> gt <|> sp <|> nl <|> tb <|> hy <|> fw <|> lr  <|> el <|> co <|> pd <|> qu

integerStringTokenParser :: Parser (Either Integer String)
integerStringTokenParser =
    try (Right <$> fromString)
    <|>
    try (Left  <$> integer)
    <|>
    (Right  <$> fail "Not permitted")

outputHTML :: Result [Either Integer String] -> String
outputHTML  =  
    \case 
        Success c -> 
                    foldr (\e a ->
                        case e of
                            (Left lft) -> show lft++a
                            (Right rgt) ->  rgt++a) "" c
        (Failure (ErrInfo e _)) -> show e

skip :: CharParsing m => m ()
skip = skipMany $ oneOf "\n\\s\t\r"

main :: IO ()
main = do
    let result = parseString (many integerStringTokenParser) mempty inputHTML
    putStrLn $ outputHTML result

    -- print multiline

    -- print eitherOr

