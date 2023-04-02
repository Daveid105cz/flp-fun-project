-- Projekt: flp22-fun
-- Autor: David Podeszwa (xpodes05)
-- Rok: 2023
module Main where

import System.Environment (getArgs)
import System.Random

import ParseInput
    ( parseCurve,
      parseSigningInfo,
      parseSwitch,
      parseVerifyInfo,
      Switch(..) )
import Ecdsa
    ( calculatePublicKey, generatePrivateKey, sign, verify )
import Types
    ( Curve(n),
      Keys(Keys),
      Signature,
      SigningInfo(SigningInfo),
      VerifyInfo(vHash, vCurve, vPublicKey, vSignature) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sw,filename] -> do -- file input
        let switch = parseSwitch sw
        case switch of
            Just a -> do
                fileContent <- readFile filename
                makeWork a fileContent
            Nothing -> error "Invalid switch on input"
    [sw] -> do  -- stdin input
        let switch = parseSwitch sw
        case switch of
            Just a -> do 
                stdinContent <- getContents
                makeWork a stdinContent
            Nothing -> error "Invalid switch on input"
    _   -> do
        error "Invalid arguments"


-- | Get curve from input
-- If input is invalid, error is thrown
getCurve :: Monad m => String -> m Curve
getCurve textInput = do
    parsed <- parseCurve textInput
    case parsed of
        Right curve -> return curve
        Left _ -> error "Invalid input"

-- | Get signing info from input
-- If input is invalid, error is thrown
getSigningInfo :: Monad m => String -> m SigningInfo
getSigningInfo textInput = do
    parsed <- parseSigningInfo textInput
    case parsed of
        Right signInfo -> return signInfo
        Left _ -> error "Invalid input"


-- | Get verify info from input
-- If input is invalid, error is thrown
getVerifyInfo :: Monad m => String -> m VerifyInfo
getVerifyInfo textInput = do
    parsed <- parseVerifyInfo textInput
    case parsed of
        Right verifyInfo -> return verifyInfo
        Left _ -> error "Invalid input"

-- | Generate new keys
-- Takes one argument - curve from which keys are generated
generateNewKeys :: Curve -> IO Keys
generateNewKeys curve = do
    generator <- newStdGen
    let privateKey = generatePrivateKey generator (n curve)
    let publicKey = calculatePublicKey curve privateKey
    return (Keys privateKey publicKey)

-- | Sign hash
-- Takes one argument - signing info and generates signature
signHash :: SigningInfo -> IO Signature
signHash (SigningInfo curve keys hash) = do
    generator <- newStdGen
    let signResult = sign generator curve keys hash
    return signResult

-- | Make work
-- Decides which action to take based on switch and then completes that action with given input
makeWork :: Switch -> String -> IO()
makeWork switch textInput = do
    case switch of
        Info -> do 
            curve <- getCurve textInput   
            print curve

        KeyGen -> do
            curve <- getCurve textInput   
            keys <- generateNewKeys curve
            print keys 
        Sign -> do
            signInfo <- getSigningInfo textInput
            s <- signHash signInfo
            print s
        Verify -> do
            verifyInfo <- getVerifyInfo textInput
            let result = verify (vCurve verifyInfo) (vPublicKey verifyInfo) (vSignature verifyInfo) (vHash verifyInfo)
            print result

