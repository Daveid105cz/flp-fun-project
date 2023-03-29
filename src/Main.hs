
module Main where

import System.Environment (getArgs)
import System.Random

import ParseInput
import Ecdsa
import Types
import qualified Control.Monad.IO.Class


main :: IO ()
main = do
  args <- getArgs
  case args of
    [sw,filename] -> do
        let switch = parseSwitch sw
        case switch of
            Just a -> do
                fileContent <- readFile filename
                makeWork a fileContent
            Nothing -> error "Invalid switch on input"
    [sw] -> do
        let switch = parseSwitch sw
        case switch of
            Just a -> do 
                stdinContent <- getContents
                makeWork a stdinContent
            Nothing -> error "Invalid switch on input"
    _   -> do
        error "Invalid arguments"


getCurve :: Monad m => String -> m Curve
getCurve textInput = do
    parsed <- parseCurve textInput
    case parsed of
        Right curve -> return curve
        Left _ -> error "Invalid input"

getSigningInfo :: Monad m => String -> m SigningInfo
getSigningInfo textInput = do
    parsed <- parseSigningInfo textInput
    case parsed of
        Right signInfo -> return signInfo
        Left _ -> error "Invalid input"

getVerifyInfo :: Monad m => String -> m VerifyInfo
getVerifyInfo textInput = do
    parsed <- parseVerifyInfo textInput
    case parsed of
        Right verifyInfo -> return verifyInfo
        Left _ -> error "Invalid input"


generateNewKeys :: Control.Monad.IO.Class.MonadIO m => Curve -> m Keys
generateNewKeys curve = do
    generator <- newStdGen
    let privateKey = generatePrivateKey generator (n curve)
    let publicKey = calculatePublicKey curve privateKey
    return (Keys privateKey publicKey)

signHash :: Control.Monad.IO.Class.MonadIO m => SigningInfo -> m Signature
signHash (SigningInfo curve keys hash) = do
    generator <- newStdGen
    let signResult = sign generator curve keys hash
    return signResult

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

