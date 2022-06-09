-- This file is a mirror of app/Main.hs, to generate haddock documentation for it via stack.

{-|
Module      : Main
Description : CSVlyr command-line interface
Copyright   : (c) Kacper Grzymkowski, 2022
Stability   : experimental
Portability : POSIX

This module contains the commad-line interface definitions for the CSVlyr program.

Example usage:

> $ csvlyr --script="select(Species, Sepal.Length)" --input="iris.csv" --output="out.csv"

> $ csvlyr --programfile="script.csvlyr" --input="iris.csv" --output="out.csv"


-}

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



-- | Data definition of command line arguments using "cmdargs".
-- @input@ and @output@ are required, and only one of @script@, @programfile@.
-- Otherwise, --help, --version will print the appropriate information and exit.
data CSVlyr = CSVlyr {
    script :: Maybe String,      -- ^ Script to run, passed as a literal string.
    programFile :: Maybe String, -- ^ Script to run, sourced from a file.
    input :: Maybe String,       -- ^ Input CSV file.
    output :: Maybe String       -- ^ Output CSV file.
    } deriving (Show, Data, Typeable)


-- | Help and information definition of command line arguments using "cmdargs".
lyrArgs :: CSVlyr
lyrArgs = CSVlyr {
    script = def &= help "Script to run" &= typ "SCRIPT",
    programFile = def &= help "File containing the script" &= typFile,
    input = def &= help "Input CSV file" &= typFile ,
    output = def &= help "Output CSV file" &= typFile
    } &= help "CSVlyr help" &= summary "CSVlyr v0.1"

-- | Shorthand for printing errors
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- | 'handleScriptLoading' handles the xor relation between 'script' and 'programFile' arguments.
-- Returns a script to be interpreted further or printing a message and exiting.
handleScriptLoading :: Maybe String -> Maybe String -> IO String
handleScriptLoading sArg sfArg = case (sArg, sfArg) of
        (Just s,  Just sf) -> putErrLn "Provide only one of script, programFile arguments" >> exitFailure >> pure ""
        (Nothing, Just sf) -> readFile' sf
        (Just s,  Nothing) -> pure s
        (Nothing, Nothing) -> putErrLn "Missing arguments. Run with --help for info." >> exitFailure >> pure ""

-- | Program entrypoint. Parses the command line argument and passes the results to 'runTransform'.
main :: IO ()
main = do
    arguments <- cmdArgs lyrArgs
    script <- handleScriptLoading (script arguments) (programFile arguments)
    case (input arguments, output arguments) of
        (Just i, Just o) -> runTransform script i o
        (_, _) -> putErrLn "Missing arguments. Run with --help for info."