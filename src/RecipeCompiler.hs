{-|
Module      : RecipeCompiler
Description : CSVlyr compilator to conduit pipepline
Copyright   : (c) Kacper Grzymkowski, 2022
Stability   : experimental
Portability : POSIX

This module contains compilers from CSVlyr 'Recipe' into a 'ConduitT' transform.
-}

module RecipeCompiler (compileTransform, compileRecipe, runTransform) where

import Language
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.CSV.Conduit
import qualified Data.Map as M
import qualified Language as L
import Data.Either (fromRight)
import Conduit (MonadTrans(lift), ResourceT)
import System.Exit (exitFailure)


-- | 'compileTransform' compiles a 'Transform' into 'ConduitT' transform to be run in a pipeline.
compileTransform :: L.Transform -> ConduitT (MapRow String) (MapRow String) (ResourceT IO) ()
compileTransform (Id) = CC.map id
compileTransform (Select cs) = CC.map (`M.intersection` M.fromList [(c, ()) | c <- cs])
compileTransform (Deselect cs) = CC.map (`M.difference` M.fromList [(c, ()) | c <- cs])
compileTransform (Head num) = CC.take num
compileTransform (Filter p) = CC.filter p
compileTransform (Mutate m) = CC.map m

-- | 'compileRecipe' compiles 'Recipe' into a single 'ConduitT' transform 
-- by compiling each transform using 'compileTransform' and then folding the resulting list with the "Conduit" fuse operator '.|'. 
compileRecipe :: L.Recipe -> ConduitT (MapRow String) (MapRow String) (ResourceT IO) ()
compileRecipe = foldr ((.|) . compileTransform) (CC.map id)

-- | 'runTransform' will attempt to parse a given script and execute it. 
-- If the given script is invalid, then a pretty-printed error will be printed to the standard output. 
runTransform :: 
  String -> -- ^ CSVlyr script. See "Language".
  String -> -- ^ Path to the input .csv file.
  String -> -- ^ Path to the output .csv file.
  IO ()
runTransform recipeString inputFile outputFile = do
    case L.parseRecipe recipeString of
      Left s -> (putStrLn $ "Recipe compile error\n" ++ s) >> exitFailure
      Right parsedRecipe -> 
        runConduitRes $
            CC.sourceFile inputFile .|
            intoCSV defCSVSettings .|
            compileRecipe parsedRecipe .|
            (writeHeaders (CSVSettings ',' Nothing) >> fromCSV (CSVSettings ',' Nothing)) .|
            CC.sinkFile outputFile

runTest :: IO ()
runTest = runTransform 
  "filter(Species == 'versicolor', Sepal.Length <= 2*Sepal.Width) |> select(Species, Sepal.Length, Sepal.Width)" 
  "test_data/iris.csv" 
  "output.csv"