
module
    IPv6AddressParser
        where

import Text.Trifecta
import Control.Applicative
import Data.Char (isHexDigit)
import Data.List

    {-
        An IPv6 (Internet Protocol version 6) address is a unique numerical "label" 
        assigned to each device participating in a computer network 
        that uses the Internet Protocol for communication. 

        * 128 bits long
        * 8 hexs (each 16 bits)
        * separated by colons
        * leading zeros within each group can be omitted
        * sequence of consecutive groups of zeros can be replaced with a double colon :: 
        (this can be done only once) 

        A hex is a group of four hexadecimal digits.
        Each hexadecimal digit represents 4 bits. 
        Therefore, a hex represents a total of 16 bits.

            2001:0db8:85a3:0000:0000:8a2e:0370:7334

        Example with leading zeros included:
            2001:0db8:85a3:0000:0000:8a2e:0370:7334

        Example with leading zeros omitted:
            2001:db8:85a3::8a2e:370:7334
    -}

data IPv6 = 
    IPv6 String String String String String String String String
    
instance Show IPv6 
    where
        show :: IPv6 -> String
        show (IPv6 _7 _6 _5 _4 _3 _2 _1 _0) = 
            "IPv6 " ++ intercalate ":" [_7,  _6, _5, _4, _3, _2, _1, _0]

parseIPv6 :: Parser (Either String IPv6)
parseIPv6 = 
    hexGroupFromElements <$> many parseOneHextetElement
        where
            hexGroupFromElements :: [String] -> Either String IPv6
            hexGroupFromElements grps =
                if "????" `elem` grps
                then 
                    Left "Invalid IPv6"
                else
                    let [_7, _6, _5, _4, _3, _2, _1, _0] =  if length grps < 8 then insertEmptyHextets grps else grps
                    in Right (IPv6 _7 _6 _5 _4 _3 _2 _1 _0)
            
            insertEmptyHextets :: [String] -> [String]
            insertEmptyHextets gs =
                let h = takeWhile (/= "0000") gs
                    m = replicate (8 - length gs) "0000"
                    t = dropWhile (`elem` h) gs
                in h ++ m ++ t

parseOneHextetElement :: Parser String
parseOneHextetElement =
    try ((some (satisfy isHexDigit) <* eof) >>= paddWithZeros) 
    <|> 
    try ((many (satisfy isHexDigit) <* string ":") >>= paddWithZeros) 
    <|> 
        (string "::" >> pure "0000" ) 
    <|>
        (anyChar >> pure "????" ) 
    where
        paddWithZeros :: Applicative f => [Char] -> f String
        paddWithZeros  [] = pure "0000"
        paddWithZeros hs = pure $ replicate (4 - length hs) '0' ++ hs

asInteger :: IPv6  -> Integer
asInteger (IPv6 hex7 hex6 hex5 hex4 hex3 hex2 hex1 hex0)  =
    sum [ 2 ^ (16 * 7) * hexStrToInteger hex7,
          2 ^ (16 * 6) * hexStrToInteger hex6,
          2 ^ (16 * 5) * hexStrToInteger hex5,
          2 ^ (16 * 4) * hexStrToInteger hex4,
          2 ^ (16 * 3) * hexStrToInteger hex3,
          2 ^ (16 * 2) * hexStrToInteger hex2,
          2 ^ (16 * 1) * hexStrToInteger hex1,
          2 ^ (16 * 0) * hexStrToInteger hex0]        
    where
        hexStrToInteger  :: String -> Integer
        hexStrToInteger [h3, h2, h1, h0] =
            (hexCharToInteger h3 * 16^3) +
            (hexCharToInteger h2 * 16^2) +
            (hexCharToInteger h1 * 16^1) +
            (hexCharToInteger h0 * 16^0)
        hexStrToInteger  _ =  65535 + 1

        hexCharToInteger :: Char -> Integer
        hexCharToInteger hex =
            case hex of '0'->0;'1'->1;'2'->2;'3'->3;'4'->4;'5'->5;'6'->6;'7'->7;'8'->8;'9'->9;'A'->10;'a'->10;'B'->11;'b'->11;'C'->12;'c'->12;'D'->13;'d'->13;'E'->14;'e'->14;'F'->15;'f'->15;_-> undefined -- the function should return: Either Error Integer
            
main :: IO()
main = do

    putStrLn "\nIPv6 from string"
    renderResult $ parseString parseIPv6 mempty "FE80:000:000:0000:0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::0:0:0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::::0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "2001:0DB8:0:00:8:0800:200C:417A"
    renderResult $ parseString parseIPv6 mempty "FE80:A:0A:000A:202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "E80:000A:000A:000A:0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80:A0:000A:000A:0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80:AB0:000A:000A:0202:B3FF:FE1E:8329"

    putStrLn "\nIPv6 to decimal"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "FE80:00:000:0000:0202:B3FF:FE1E:8329"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "FE80::0:0:0202:B3FF:FE1E:8329"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "FE80::0202:B3FF:FE1E:8329"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "FE80::::0202:B3FF:FE1E:8329"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "2001:0DB8:0:00:8:0800:200C:417A"
    renderResult $ parseString ((fmap . fmap) asInteger parseIPv6) mempty "EC26:F:0B:000B:212:A4CF:AE2D:548"

    putStrLn "\nInvalid IPv6"
    renderResult $ parseString parseIPv6 mempty "GE80:000:000:0000:0202:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::0:0:0G02:B3FF:FE1E:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::0202:B3FF:F1GE:8329"
    renderResult $ parseString parseIPv6 mempty "FE80::::0202:B3FF:FE1E:832G"
    
    where 
        renderResult :: (Show a)=> Result (Either String a) -> IO ()
        renderResult r  =
            case r of 
                (Success (Right r)) -> print r 
                (Success (Left l)) -> print l
                (Failure (ErrInfo e d)) ->  print  e
