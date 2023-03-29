-- Projekt: flp22-fun
-- Autor: David Podeszwa (xpodes05)
-- Rok: 2023
module ParseInput
    ( parseSwitch, parseCurve, parseSigningInfo, parseVerifyInfo,  Switch(..)
    ) where

import Text.Parsec
    ( ParseError,
      char,
      digit,
      hexDigit,
      spaces,
      string,
      count,
      many1,
      (<|>),
      parse,
      try,
      Parsec )
import Numeric (readHex, readDec)
import Types
    ( Curve(Curve),
      Keys(Keys),
      Point(Point),
      PrivateKey(PrivateKey),
      PublicKey(PublicKey),
      Signature(Signature),
      SigningInfo(SigningInfo),
      VerifyInfo(VerifyInfo) )

data Switch = Info | KeyGen | Sign | Verify deriving (Enum, Show)

-- | Parse switch from args
parseSwitch :: String -> Maybe Switch
parseSwitch "-i" = Just Info
parseSwitch "-k" = Just KeyGen
parseSwitch "-s" = Just Sign
parseSwitch "-v" = Just Verify
parseSwitch _ = Nothing

-- | Parses the hexadecimal marker
preHexa :: Parsec String () Char
preHexa = do
    char '0'
    char 'x' <|> char 'X'

-- | Parses a hexadecimal number
integerHexa :: Parsec String () Integer
integerHexa = do
    preHexa
    allHexDigits <- many1 hexDigit
    case readHex allHexDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

-- | Parses an integer in a decimal format
integer :: Parsec String () Integer
integer = do
    allDigits <- many1 digit
    case readDec allDigits of
        [(n, "")] -> return n
        _ -> error "Invalid number"

-- | Parses an integer with a name
-- The number can be in a hexadecimal or decimal format
integerWithName :: String -> Parsec String () Integer
integerWithName propertyName = do
    string propertyName
    string ":"
    spaces
    try integerHexa <|> integer

-- | Parses a point
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

-- | Parses a curve
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

-- | Parses a curve using Parsec given the input string
parseCurve :: Monad m => String -> m (Either ParseError Curve)
parseCurve textInput = do 
    return $ parse curve "" textInput

-- | Parses a public key using Parsec given the input string
publicKey :: Parsec String () PublicKey
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

-- | Parses a private key and public key
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

-- | Parses a hash
hash :: Parsec String () Integer
hash = integerWithName "Hash"

-- | Parses a signing info
signingInfo :: Parsec String () SigningInfo
signingInfo = do
    c <- curve
    spaces
    k <- keys
    spaces
    h <- hash
    spaces
    return (SigningInfo c k h)

-- | Parses a signing info using Parsec given the input string
parseSigningInfo :: Monad m => String -> m (Either ParseError SigningInfo)
parseSigningInfo textInput = do 
    return $ parse signingInfo "" textInput

-- | Parses a public key
publicKeyOnly :: Parsec String () PublicKey
publicKeyOnly = do
    string "PublicKey"
    spaces
    char '{'
    spaces
    string "Q:"
    spaces
    q <- publicKey
    spaces
    char '}'
    return q

-- | Parses a message signature
msgSignature :: Parsec String () Signature
msgSignature = do
    string "Signature"
    spaces
    char '{'
    spaces
    r <- integerWithName "r"
    spaces
    s <- integerWithName "s"
    spaces
    char '}'
    return (Signature r s)

-- | Parses a verify info
verifyInfo :: Parsec String () VerifyInfo
verifyInfo = do
    c <- curve
    spaces
    sig <- msgSignature
    spaces
    p <- publicKeyOnly
    spaces
    h <- hash
    spaces
    return (VerifyInfo c sig p h)

-- | Parses a verify info using Parsec given the input string
parseVerifyInfo :: Monad m => String -> m (Either ParseError VerifyInfo)
parseVerifyInfo textInput = do 
    return $ parse verifyInfo "" textInput
    