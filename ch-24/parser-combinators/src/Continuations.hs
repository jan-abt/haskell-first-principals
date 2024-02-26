
module
    Continuations
        where

import Text.Trifecta
import Control.Applicative

-- ================================================ --    
-- =============== PARSE INTEGER ================== --    
-- ================================================ --    

parseDigit :: Parser Char
parseDigit = satisfy (\c -> c `elem` ['0','1'..'9'])

parseNegation :: Parser Char
parseNegation = satisfy (== '-')

readAsInteger :: String -> Integer
readAsInteger n = read n :: Integer

parseInteger :: Parser Integer
parseInteger =
    readAsInteger <$> some parseDigit
   <|>
    negate . readAsInteger <$> (parseNegation >> some parseDigit)

-- ================================================ --    
-- ============== PARSE PHONE NUMBER ============== --    
-- ================================================ --    

type AreaCode = Int -- aka area code 
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber Integer Integer Integer
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined

tryDropPrefix :: Parser Char
tryDropPrefix = try (char '1' >> char '-') 

parseIntegers :: Parser [Integer]
parseIntegers =  some (many (oneOf "-() ") >>  parseInteger)

makePhoneNumber :: Monad m => [Integer] -> m PhoneNumber
makePhoneNumber digs
        | length digs == 3  =  makeInsanceFromParts digs
        | otherwise  =  makeInsanceFromOne digs
   where     
      makeInsanceFromParts parts = 
        return (PhoneNumber (head parts) (parts!!1) (parts!!2))
      makeInsanceFromOne one =    
        let xs = show $ head one
            a = take 3 xs 
            b = take 3 (drop 3 xs)
            c = take 4 (drop 6 xs)
        in return $ PhoneNumber (read a :: Integer) (read b:: Integer) (read c :: Integer)

parseNumber :: Parser PhoneNumber
parseNumber =
    (tryDropPrefix >> parseIntegers >>= makePhoneNumber ) 
    <|> 
    (parseIntegers >>=  makePhoneNumber )

     
main :: IO ()
main = do

    -- ================================================ --    
    
    print $ parseString parseDigit mempty "123ABT"
    print $ parseString parseInteger mempty "123ABT"
    print $ parseString parseInteger mempty "-123ABT"

    -- ================================================ --    
    
    print $ parseString  parseNumber mempty "1-917-512-2767"
    print $ parseString  parseNumber mempty "917-512-2767"
    print $ parseString  parseNumber mempty "(917)-512-2767"
    print $ parseString  parseNumber mempty "(917) 512 2767"
    print $ parseString  parseNumber mempty "(917) 512 2767"
    print $ parseString  parseNumber mempty "917 512 2767"
    print $ parseString  parseNumber mempty "917 512 2767"
    print $ parseString  parseNumber mempty "9175122767"

    -- ================================================ --    
