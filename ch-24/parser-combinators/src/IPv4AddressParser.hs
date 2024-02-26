
module
    IPv4AddressParser
        where

import Text.Trifecta
import Control.Applicative
import Data.List

import Numeric (showHex)

import IPv6AddressParser hiding (asInteger, main)
{-


    An IPv4 address, or Internet Protocol version 4 address, is a 
    numerical "label" to uniquely identiy and locate devices that are
    participating in a computer network , which uses the Internet Protocol 
    for communication.

    IPv4 addresses are 32-bit numerical "labels" written in the form of 
    * 32 bits long
    * four octets (each 8 bits)
    * separated by dots 

    An "octet" is an 8-bit binary sequence.
    There are 4 such octets, making up the total of 32 bits.
    In decimal notation, an octet is represented as a value ranging from 0 to 255.
    Each octet is separated by a dot.
     
        11111111.11111111.11111111.11111111 or 255.255.255.255

        Power of two    2^7	2^6	2^5	2^4	2^3	2^2	2^1	2^0	
        Decimal space   128	64	32	16	8	4	2	1  (sums to 255)
    
    An IPv4 address can be represented as a decimal number.
    For example, the IPv4 address "172.16.254.1" can be 
    represented as the decimal number 2886794753.
    
    (172 * 256^3) + (16 * 256^2) + (254 * 256^1) + (1 * 256^0)

-}


data IPv4 =
    IPv4 Integer Integer Integer Integer

instance Show IPv4
    where
        show :: IPv4 -> String
        show (IPv4  _3 _2 _1 _0) =
            "IPv4 " ++ intercalate "." [show _3, show _2,show _1,show _0]

parseIPv4 :: Parser (Either String IPv4)
parseIPv4 =
    let ipv4Parser = try  ( integer <*  eof ) <|> ( integer <*  char '.' )
    in  ipv4Parser >>= \oct4 ->
            ipv4Parser >>= \oct3  ->
                ipv4Parser >>= \oct2 ->
                    ipv4Parser >>= \oct1 ->
                        if all valid [oct4, oct3, oct2, oct1]
                        then
                            return $ Right (IPv4 oct4 oct3 oct2 oct1)
                        else
                            return $ Left ("Cannot construct IPv4 using input " ++ (intercalate "." [show oct4, show oct3,show oct2,show oct1]) ++ " Usage: n.n.n.n where n is between 0 and 255")
                        where
                            valid n = 255 >= n &&  n >= 0

asInteger :: IPv4 -> Integer
asInteger (IPv4 octet3 octet2 octet1 octet0)  =
    let ipv4 = (octet3 * 256^3) + (octet2 * 256^2) + (octet1 * 256^1) + (octet0 * 256^0)
    in fromIntegral ipv4

asIPv6 :: IPv4 -> Result (Either String IPv6)
asIPv6 (IPv4 octet3 octet2 octet1 octet0)  =
    let _3 = if octet3 < 10 then "0" ++ showHex octet3 "" else showHex octet3 ""
        _2 = if octet2 < 10 then "0" ++ showHex octet2 "" else showHex octet2 ""
        _1 = if octet1 < 10 then "0" ++ showHex octet1 "" else showHex octet1 ""
        _0 = if octet0 < 10 then "0" ++ showHex octet0 "" else showHex octet0 ""

    in parseString parseIPv6 mempty ("0000:0000:0000:0000:0000:FFFF" ++ ":" ++ (_3 ++ _2) ++ ":" ++ (_1 ++ _0) )

main :: IO()
main = do

    putStrLn "\nValid IPv4"
    renderResult $ parseString parseIPv4 mempty "172.16.254.1"
    renderResult $ parseString parseIPv4 mempty "204.120.0.15"

    putStrLn "\nIPv4 to IPv6"
    renderResult $ parseString (fmap asIPv6 <$> parseIPv4) mempty "172.16.254.1"
    renderResult $ parseString (fmap asIPv6 <$> parseIPv4)  mempty "204.120.0.15"

    putStrLn "\nAs integer"
    renderResult $ parseString (fmap asInteger <$> parseIPv4) mempty "172.16.254.1"
    renderResult $ parseString (fmap asInteger <$> parseIPv4) mempty "204.120.0.15"

    putStrLn "\nInvalid IPv4"
    renderResult $ parseString parseIPv4 mempty "256.2.0.1.5"

    putStrLn "\nFailure to convert to decimal"
    renderResult $ parseString (fmap asInteger <$> parseIPv4) mempty "256.2.0.1"

    where
        renderResult :: (Show a)=> Result (Either String a) -> IO ()
        renderResult r  =
            case r of
                (Success (Right r)) -> print r
                (Success (Left l)) -> print l
                (Failure (ErrInfo e d)) ->  print  e
