{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module
    INIConfigParser
        where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Test.Hspec
import Text.RawString.QQ

-- parsers 0.12.3, trifecta 1.5.2 
import Text.Trifecta

-- ============================================================ --
-- ============================================================ --
-- ============================================================ --
type Comment =  ByteString

commentEx :: Comment
commentEx = "; last modified 1 April 2001 by John Doe"

commentEx' :: Comment
commentEx' = "; blah\n; woot\n \n;hah"

-- skip zero, one or more comments, starting at the beginning of the line.
skipComments :: Parser [()]
skipComments =
    let stmt = cmtSymbolParser *> cmtContentParser  <* cmtEndParser
    in many stmt <|> ( (:[]) <$> skipEOL)
        where
            cmtSymbolParser = char ';' <|> char '#'
            cmtContentParser = skipMany $ noneOf "\n" -- ignore anything except newline
            cmtEndParser = skipEOL

-- ============================================================ --
-- ============================================================ --
-- ============================================================ --

-- "[blah]" -> Section "blah"
newtype SectionHeader =
    SectionHeader String
    deriving (Eq, Ord, Show)

headerEx :: ByteString
headerEx = "[blah]"

-- combining two parsers in order to parse the SectionHeader "h"   
parseBracketPair :: Parser a -> Parser a
parseBracketPair h = char '[' *> h <* char ']'

parseSectionHeader :: Parser SectionHeader
parseSectionHeader = parseBracketPair (SectionHeader <$> some letter)

-- ============================================================ --
-- ============================================================ --
-- ============================================================ --

type Name = String
type Value = String
type Assignments = Map Name Value

assignmentEx :: ByteString
assignmentEx = "woot=1"

parseSectionAssignment :: Parser (Name, Value)
parseSectionAssignment = do
    name <- some letter
    _ <- char '='
    value <- some (noneOf " \n")
    _ <- skipEOL -- skip "end-of-line" until we stop getting newline characters
    return (name, value)

--  Skip until end of line
skipEOL :: Parser ()
skipEOL = skipMany (char '\n')

-- ============================================================ --
-- ============================================================ --
-- ============================================================ --

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|; ignore me
[states]
Chris=Texas 
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment

[section]
alias=claw
host=wikipedia.org 

[whatisit]
red=intoothandclaw

|]

data Section =
    Section SectionHeader Assignments
    deriving (Eq, Show)

newtype Config =
    Config (Map SectionHeader Assignments)
    deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n")

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    _ <- skipComments
    sectionTitle <- parseSectionHeader
    _ <- skipEOL
    assignments <- some parseSectionAssignment
    _ <- skipEOL
    return $ Section sectionTitle (M.fromList assignments)

put :: Section -> Map SectionHeader Assignments -> Map SectionHeader Assignments
put (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr put M.empty sections
    return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing


main :: IO ()
main = hspec $ do

    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseSectionAssignment mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")

    describe "SectionHeader Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString parseSectionHeader mempty headerEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (SectionHeader "blah")

    describe "Comment parsing" $
        it "Can skip a comment before a header" $ do
            let p = skipComments >> parseSectionHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (SectionHeader "blah")

    describe "Section parsing" $
        it "Can parse a simple section" $ do
            let m = parseByteString parseSection mempty sectionEx'
                r' = maybeSuccess m
                states = M.fromList [("Chris", "Texas")]
                expected' = Just (Section (SectionHeader "states") states)
            print m
            r' `shouldBe` expected'

    describe "INI parsing" $
        it "Can parse multiple sections" $ do
            let m = parseByteString parseIni mempty sectionEx''
                r' = maybeSuccess m
                sectionValues = M.fromList [ ("alias", "claw"), ("host", "wikipedia.org")]
                whatisitValues = M.fromList [("red", "intoothandclaw")]
                expected' =
                    Just (Config (M.fromList [
                                        (SectionHeader "section", sectionValues),
                                        (SectionHeader "whatisit", whatisitValues)
                                    ]
                                 )
                         )
            print m
            r' `shouldBe` expected'