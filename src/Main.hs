
module Main where

import System.Environment (getArgs)
import System.Random

import ParseInput
import Ecdsa
import Types


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


-- generateKeys :: Monad m => String -> m 
generateNewKeys curve = do
    generator <- newStdGen
    let privateKey = generatePrivateKey generator (n curve)
    -- let privateKey = PrivateKey 0xc9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e
    let publicKey = calculatePublicKey curve privateKey
    return (Keys privateKey publicKey)

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
            -- let resu = verify (vCurve verifyInfo) (vPublicKey verifyInfo) (vSignature verifyInfo) 0x94996fead5b722c3bd07360c459927976e804f869626f4897def03fa56b009e3
            let result = verify (vCurve verifyInfo) (vPublicKey verifyInfo) (vSignature verifyInfo) (vHash verifyInfo)
            print result

