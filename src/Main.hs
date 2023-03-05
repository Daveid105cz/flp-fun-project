{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Maybe (isJust)

import ParseInput
import Curves
import Text.Parsec
import Text.Parsec.Error
import System.Random
import Ecdsa
import Keys


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
            Just a -> makeWork a "Stdin"
            Nothing -> error "Invalid switch on input"
    []   -> do
        error "Invalid arguments, no switch set"



getCurve :: Monad m => String -> m Curve
getCurve textInput = do
    parsed <- parseCurve textInput
    case parsed of
        Right curve -> return curve
        Left err -> error "Invalid input"


-- generateKeys :: Monad m => String -> m 
generateNewKeys curve = do
        generator <- newStdGen
        let privateKey = generatePrivateKey generator (n curve)
        let publicKey = calculatePublicKey curve privateKey
        return (Key privateKey publicKey)

makeWork :: Switch -> String -> IO()
makeWork switch textInput = do
    case switch of
        Info -> do 
                putStrLn "Printing info"
                curve <- getCurve textInput   
                print curve

        KeyGen -> do
            putStrLn "Printing K"
            curve <- getCurve textInput   
            keys <- generateNewKeys curve
            print keys 
            -- newKey <- parseAndGenerateKey textInput
            -- print newKey
        Sign -> putStrLn "Printing S"
        Verify -> putStrLn "Printing V"

