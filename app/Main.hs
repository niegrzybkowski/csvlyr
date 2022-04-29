module Main where

import System.Environment (getArgs)
import Lib
import Language ( Transform, parseRecipe )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr 

interactiveParseRecipe :: String -> IO [Transform]
interactiveParseRecipe program = 
    case parseRecipe program of
        Left err  -> putErrLn err >> exitFailure >> pure []
        Right pro -> pure pro

main :: IO ()
main = do
    inputProgram:_ <- getArgs
    recipe <- interactiveParseRecipe inputProgram
    print recipe

