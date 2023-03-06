module ParseInput
    ( parseSwitch, parseCurve, parseSigningInfo,  Switch(..)
    ) where

import Curves
import Keys
import Text.Parsec
import Numeric (readHex, readDec)
import Ecdsa (SigningInfo(SigningInfo))

data Switch = Info | KeyGen | Sign | Verify deriving (Enum, Show)

parseSwitch :: String -> Maybe Switch
parseSwitch "-i" = Just Info
parseSwitch "-k" = Just KeyGen
parseSwitch "-s" = Just Sign
parseSwitch "-v" = Just Verify
parseSwitch _ = Nothing

preHexa = do
    char '0'
    char 'x' <|> char 'X'

integerHexa :: Parsec String () Integer
integerHexa = do
    preHexa
    allHexDigits <- many1 hexDigit
    case readHex allHexDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

integer :: Parsec String () Integer
integer = do
    allDigits <- many1 digit
    case readDec allDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

integerWithName propertyName = do
    string propertyName
    string ":"
    spaces
    try integerHexa <|> integer

point :: Parsec String () Point
point = do
    string "Point"
    spaces
    char '{'
    spaces
    x <- integerWithName "x"
    spaces
    y <- integerWithName "y"
    spaces
    char '}'
    return (Point x y)

-- Parse a curve
curve :: Parsec String () Curve
curve = do
    string "Curve"
    spaces
    char '{'
    spaces
    p <- integerWithName "p"
    spaces
    a <- integerWithName "a"
    spaces
    b <- integerWithName "b"
    spaces
    string "g:"
    spaces
    g <- point
    spaces
    n <- integerWithName "n"
    spaces
    h <- integerWithName "h"
    spaces
    char '}'
    return (Curve p a b g n h)


parseCurve :: Monad m => String -> m (Either ParseError Curve)
parseCurve textInput = do 
    return $ parse curve "" textInput

publicKey = do
    preHexa
    string "04"
    xHexDigits <- count 64 hexDigit
    let x = case readHex xHexDigits of
            [(n, "")] -> n
            _ -> error "Invalid number"
    yHexDigits <- count 64 hexDigit
    let y = case readHex yHexDigits of
            [(n, "")] -> n
            _ -> error "Invalid number"
    return (PublicKey x y)

keys :: Parsec String () Keys
keys = do
    string "Key"
    spaces
    char '{'
    spaces
    d <- integerWithName "d"
    spaces
    string "Q:"
    spaces
    q <- publicKey
    spaces
    char '}'
    return (Keys (PrivateKey d) q)

hash = integerWithName "Hash"

signingInfo = do
    c <- curve
    spaces
    k <- keys
    spaces
    h <- hash
    spaces
    return (SigningInfo c k h)

parseSigningInfo :: Monad m => String -> m (Either ParseError SigningInfo)
parseSigningInfo textInput = do 
    return $ parse signingInfo "" textInput