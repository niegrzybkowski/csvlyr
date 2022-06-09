module RecipeCompiler where

import Language
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.CSV.Conduit
import qualified Data.Map as M
import qualified Language as L
import Data.Either (fromRight)
import Conduit (MonadTrans(lift), ResourceT)
type StrictMapRow t = M.Map t t

compileTransform :: L.Transform -> ConduitT (MapRow String) (MapRow String) (ResourceT IO) ()
compileTransform (Id) = CC.map id
compileTransform (Select cs) = CC.map (`M.intersection` M.fromList [(c, ()) | c <- cs])
compileTransform (Deselect cs) = CC.map (`M.difference` M.fromList [(c, ()) | c <- cs])
compileTransform (Head num) = CC.take num
compileTransform (Filter p) = CC.filter p
compileTransform (Mutate m) = CC.map m

compileRecipe :: L.Recipe -> ConduitT (MapRow String) (MapRow String) (ResourceT IO) ()
compileRecipe = foldr ((.|) . compileTransform) (CC.map id)

runTransform :: String -> String -> String -> IO ()
runTransform recipeString inputFile outputFile = do
    case L.parseRecipe recipeString of
      Left s -> putStrLn $ "Recipe compile error\n" ++ s
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