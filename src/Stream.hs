module Stream where

import Conduit

import Control.Monad ( foldM )
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT, execStateT)
import qualified Language as L
import qualified Data.Vector as V

type Header = V.Vector String
type Record = V.Vector String
type CompiledTransform m = ConduitT Record Record m ()

idC :: (Monad m) => CompiledTransform m
idC = mapC id

translateSelector :: Header -> L.ColumnSelector -> Maybe [Int]
translateSelector header = mapM (`V.elemIndex` header)

translateHeader :: Header -> [Int] -> Header
translateHeader header idxs = V.ifilter (\i _ -> i `elem` idxs) header

translateRecord :: Record -> [Int] -> Record
translateRecord = translateHeader

translateRecordC :: (Monad m) => [Int] -> ConduitT Record Record m ()
translateRecordC idxs = mapC (`translateRecord` idxs)

compileTransform :: (Monad m) =>
    L.Transform ->
    Header ->
    Maybe (Header, ConduitT Record Record m ())
compileTransform L.Id header = Just (header, idC)

compileTransform (L.Select cs) header = do
    indices <- translateSelector header cs
    pure (translateHeader header indices, translateRecordC indices)

compileTransform (L.Head num) header = pure (header, takeC num)


compileTransform _ _ = Nothing

compileRecipe :: (Monad m) =>
    L.Recipe ->
    Header ->
    Maybe (Header, CompiledTransform m)
compileRecipe recipe header = foldM compileNext (header, idC) recipe
    where
        compileNext :: (Monad m) =>
            (Header, CompiledTransform m) ->
            L.Transform ->
            Maybe (Header, CompiledTransform m)
        compileNext (header, ct) transform =
            case compileTransform transform header of
            Nothing -> Nothing
            Just (newHeader, nextct) -> Just (newHeader, ct .| nextct)

extractCompiledRecipe :: (Monad m) =>
    Maybe (Header, CompiledTransform m) -> CompiledTransform m
extractCompiledRecipe cres = case cres of
    Nothing -> idC
    Just (_, b) -> b

recTest :: (Monad m) => ConduitT () Record m ()
recTest = yieldMany [V.fromList [show c, "a"] | c <- [1..10]]

recipeTest :: (Monad m) => Maybe (Header, CompiledTransform m)
recipeTest = compileRecipe [L.Id, L.Select ["a"], L.Head 3] (V.fromList ["a", "b"])

runTest :: IO ()
runTest =
    runConduit (
        recTest .| 
        extractCompiledRecipe (recipeTest) .|
        sinkList
        ) >>= print  

{-

let testTransform = "select(a, c) |> head(3)"
let Right parsedRecipe = parseRecipe testTransform 

let header = V.fromList ["a", "b", "c"]
let records = [V.fromList [show c, "text",(show c) ++ ".1"] | c <- [1..10]]

runConduit (yieldMany records  .| extractCompiledRecipe (compileRecipe parsedRecipe header) .| sinkList )


runResourceT $ transformCSV defCSVSettings (sourceFile "test_data/iris.csv") myProcessor (sinkFile "output.csv")
-}