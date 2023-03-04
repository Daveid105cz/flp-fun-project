{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Maybe (isJust)

import ParseInput
import Types
import Text.Parsec
import Text.Parsec.Error


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

doParseForInfo :: Monad m => String -> m (Either ParseError Curve)
doParseForInfo textInput = do 
    return $ parse parseCurve "" textInput

makeWork :: Switch -> String -> IO()
makeWork switch textInput = do
    case switch of
        Info -> do 
                putStrLn "Printing info"
                parsed <- doParseForInfo textInput
                case parsed of
                    Right curve -> print curve
                    Left err -> print err

        KeyGen -> putStrLn "Printing K"
        Sign -> putStrLn "Printing S"
        Verify -> putStrLn "Printing V"

