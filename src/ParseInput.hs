module ParseInput
    ( parseSwitch, parseCurve
    ) where

import Types
import Text.Parsec
import Numeric (readHex, readDec)

parseSwitch :: String -> Maybe Switch
parseSwitch "-i" = Just Info
parseSwitch "-k" = Just KeyGen
parseSwitch "-s" = Just Sign
parseSwitch "-v" = Just Verify
parseSwitch _ = Nothing

bigNumberHexa :: Parsec String () Integer
bigNumberHexa = do
    char '0'
    char 'x' <|> char 'x'
    allHexDigits <- many1 hexDigit
    case readHex allHexDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

bigNumber :: Parsec String () Integer
bigNumber = do
    allDigits <- many1 digit
    case readDec allDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

bigNumberWithName propertyName = do
    string propertyName
    string ":"
    spaces
    try bigNumberHexa <|> bigNumber

parsePoint :: Parsec String () Point
parsePoint = do
    string "Point"
    spaces
    char '{'
    spaces
    x <- bigNumberWithName "x"
    spaces
    y <- bigNumberWithName "y"
    spaces
    char '}'
    return (Point x y)

-- Parse a curve
parseCurve :: Parsec String () Curve
parseCurve = do
    string "Curve"
    spaces
    char '{'
    spaces
    p <- bigNumberWithName "p"
    spaces
    a <- bigNumberWithName "a"
    spaces
    b <- bigNumberWithName "b"
    spaces
    string "g:"
    spaces
    g <- parsePoint
    spaces
    n <- bigNumberWithName "n"
    spaces
    h <- bigNumberWithName "h"
    spaces
    char '}'
    return (Curve p a b g n h)