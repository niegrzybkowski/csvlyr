{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Environment (getArgs)
import Language ( Transform, parseRecipe )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, readFile')
import RecipeCompiler (runTransform)
import Data.Data (Typeable, Data)

import System.Console.CmdArgs

data CSVlyr = CSVlyr {
    script :: Maybe String,
    programFile :: Maybe String,
    input :: Maybe String,
    output :: Maybe String
    } deriving (Show, Data, Typeable)

lyrArgs :: CSVlyr
lyrArgs = CSVlyr {
    script = def &= help "Script to run" &= typ "SCRIPT",
    programFile = def &= help "File containing the script" &= typFile,
    input = def &= help "Input CSV file" &= typFile ,
    output = def &= help "Output CSV file" &= typFile
    } &= help "CSVlyr help" &= summary "CSVlyr v0.1"

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

interactiveParseRecipe :: String -> IO [Transform]
interactiveParseRecipe program =
    case parseRecipe program of
        Left err  -> putErrLn err >> exitFailure >> pure []
        Right pro -> pure pro

handleScriptLoading :: Maybe String -> Maybe String -> IO String
handleScriptLoading sArg sfArg = case (sArg, sfArg) of
        (Just s,  Just sf) -> putStrLn "Provide only one of script, programFile arguments" >> exitFailure >> pure ""
        (Nothing, Just sf) -> readFile' sf
        (Just s,  Nothing) -> pure s
        (Nothing, Nothing) -> putStrLn "Missing arguments. Run with --help for info." >> exitFailure >> pure ""

main :: IO ()
main = do
    arguments <- cmdArgs lyrArgs
    script <- handleScriptLoading (script arguments) (programFile arguments)
    case (input arguments, output arguments) of
        (Just i, Just o) -> runTransform script i o
        (_, _) -> putStrLn "Missing arguments. Run with --help for info."