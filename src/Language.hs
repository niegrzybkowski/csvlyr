{-|
Module      : Language
Description : CSVlyr language parsing module
Copyright   : (c) Kacper Grzymkowski, 2022
Stability   : experimental
Portability : POSIX

This module contains parsers for the CSVlyr transformation language.
This language allows for easy transformation of tabular CSV files, such as the well-known iris dataset.
Example script:

@

filter(Species == \'setosa\') |>
select(Species, Petal.Length, Petal.Width) |>
mutate(
    Species.Pigish = Species ++ \'y\',
    Petal.Area = Petal.Length*Petal.Width 
) |>
filter( 
    Petal.Area >= 2.0e-1
)

@
-}

module Language (
    Transform(..),
    Recipe,
    ColumnSelector,
    MapRow,
    parseRecipe,
    recipe,
    transform,
    columnSelector,
    column,
    predicates,
    stringPredicate,
    numericPredicate,
    mutations,
    stringMutation,
    numericMutation,
    specialChars
    ) where

import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.CSV.Conduit (MapRow)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Map as M
import Data.Conduit (ConduitT)
import Control.Monad.Combinators.Expr ( Operator(InfixL), makeExprParser )
import Control.Monad (MonadPlus)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Control.Monad.Trans.State.Strict ( StateT )
import Control.Monad.Trans.Class (lift)


-- | Data definition describing supported data transforms.
data Transform = Id                           -- ^ Identity transform.
    | Select ColumnSelector                   -- ^ Subset of columns.
    | Deselect ColumnSelector                 -- ^ Column negative subset.
    | Head Int                                -- ^ Row subset of first n elements.
    | Filter (MapRow String -> Bool)          -- ^ Subset of rows by a predicate, the row is retained when the predicate is true.
    | Mutate (MapRow String -> MapRow String) -- ^ Creation or replacement of a column using the contained function.

instance Show Transform where
    show Id = "Id()"
    show (Select cs) = "Select("++ show cs ++ ")"
    show (Deselect cs) = "Deselect("++ show cs ++ ")"
    show (Head n) = "Head(" ++ show n ++ ")"
    show (Filter _) = "Filter(<???>)"
    show (Mutate _) = "Mutate(<???>)"

-- | A 'Recipe' is a list of transforms to run in sequence.
type Recipe = [Transform]

-- | A 'ColumnSelector' is a list of column names separated by commas.
type ColumnSelector = [String]
type Parser = Parsec Void String

-- | Primary parser entrypoint. Parses a 'String' and returns a compiled 'Recipe' or a pretty-printed error message
-- Language grammar is a series of transforms separated by the pipe symbol \'|>\'.
-- Transforms are in C-like format of @transformName(transformParameters)@.
parseRecipe :: String -> Either String Recipe
parseRecipe s = case (runParser (recipe <* eof) "" s) of
  Left err -> Left $ errorBundlePretty err
  Right fine -> Right fine

-- | Parser for 'Recipe', a list of transforms separated by the pipe symbol \'|>\'. (see 'transform')
recipe :: Parser Recipe
recipe = sepBy transform (symbol "|>")

-- | Parser for 'Transform', in C-like format of @transformName(transformParameters)@.
-- For individual transforms see:
-- * column
transform :: Parser Transform
transform = choice [
        Id       <$  (symbol "id"       >> symbol "(" >> symbol ")"),
        Select   <$> (symbol "select"   *> parens columnSelector),
        Deselect <$> (symbol "deselect" *> parens columnSelector),
        Head     <$> (symbol "head"     *> parens (lexeme L.decimal)),
        Filter   <$> (symbol "filter"   *> parens predicates),
        Mutate   <$> (symbol "mutate"   *> parens mutations)
    ] <?> "transform"

-- | List of special characters used in the language, which are not allowed in non-escaped scenarios.
specialChars :: [Char]
specialChars = " ,()|+-*/='\"`"

quotedColumn :: Parser String
quotedColumn = between (symbol "`") (symbol "`") (lexeme $ some (noneOf "`"))

-- | Columns can be selected by their name without any quoting if the name doesn't contain special characters (see 'specialChars').
-- Columns may also be selected by quoting them with the backtick character '`'.
column :: Parser String
column = lexeme (some (noneOf specialChars)) <|> quotedColumn <?> "columnName"

-- | Parser for 'ColumnSelector', a list of column names separated by commas (see 'column').
-- For example, on the iris dataset: @select(Species, Sepal.Length, \`Sepal.Width\`)@ will subset the three columns.
columnSelector :: Parser [String]
columnSelector = sepBy column (symbol ",")

-- | Predicates of type either string or numeric allow for subsetting rows.
-- Multiple predicates may be provided, separated by a comma ',', in which case they will be evaluated with a logical AND.
-- Predicate is in the general format of @/expression operator expression/@.
-- For example, on the iris dataset: @ filter(Species == \'setosa\', Sepal.Length >= 2.0) @ 
-- will select only observations of species setosa and with a sepal length of at least 2.
predicates :: Parser (MapRow String -> Bool)
predicates = sepBy (try stringPredicate <|> try numericPredicate) (symbol ",") >>=
    \listOfPredicates -> pure (\row -> and $ listOfPredicates <*> [row])

-- | Predicate of type 'String' allowing concatenation expressions '++', 
-- string literals quoted in single quotes \',
-- quoted or unqouted columns (see 'column') ,
-- and equality operations ('==', '!=')
stringPredicate :: Parser (MapRow String -> Bool)
stringPredicate = do
    lhs <- stringExpr
    op <- choice [
        (==) <$ symbol "==",
        (/=) <$ symbol "!="
        ]
    rhs <- stringExpr
    pure (\row -> lhs row `op` rhs row)

-- | Predicate of type 'Double' allowing 
-- basic arithmetic expressions ('+','-','*','/'), 
-- numeric literals (e.g. 2, 2.0, 1.3e3),
-- quoted or unqouted columns (see 'column') ,
-- and comparison operations (e.g. '==', '<', '!=')
numericPredicate :: Parser (MapRow String -> Bool)
numericPredicate = do
    lhs <- numericExpr
    op <- choice [
        (==) <$ symbol "==",
        (>=) <$ symbol ">=",
        (> ) <$ symbol ">",
        (<=) <$ symbol "<=",
        (< ) <$ symbol "<",
        (/=) <$ symbol "!="
        ] <?> "binary logical operator"
    rhs <- numericExpr
    pure (\row -> lhs row `op` rhs row)

-- | Mutations allow creating new columns and replacing existing ones. They are either of 'String' or 'Double' type.
-- Mutations have the general format of @mutate(/columnName/ = /expression/)@.
-- For example: @mutate(Petal.Area = Petal.Length * Petal.Width * 0.5)@ will create a new column named @Petal.Length@, 
-- which is equal to half of @Petal.Length * Petal.Width@ for all observations.
mutations :: Parser (MapRow String -> MapRow String)
mutations = sepBy (try numericMutation <|> try stringMutation) (symbol ",") >>=
    \listOfMutations -> pure (foldl (.) id listOfMutations)

-- | Mutation of type 'String' allowing 
-- concatenation expressions ('++'),
-- quoted or unqouted columns (see 'column') 
-- and string literals (quoted with \').
stringMutation :: Parser (MapRow String -> MapRow String)
stringMutation = (do
    targetColumn <- column
    symbol "="
    rhs <- stringExpr
    pure (\row -> M.insert targetColumn (rhs row) row)) <?> "string mutation"

-- | Predicate of type 'Double' allowing 
-- basic arithmetic expressions ('+','-','*','/'), 
-- numeric literals (e.g. 2, 2.0, 1.3e3),
-- and quoted or unqouted columns (see 'column').
numericMutation :: Parser (MapRow String -> MapRow String)
numericMutation = do
    lhs <- column
    symbol "="
    rhs <- numericExpr
    pure (\row -> M.insert lhs (show $ rhs row) row) <?> "numeric mutation"

stringLiteral :: Parser String
stringLiteral = between (symbol "'") (symbol "'") (lexeme $ some (noneOf "'"))

rhsValue :: Parser (MapRow String -> String)
rhsValue = (const <$> stringLiteral) <|> ((\col row  -> row M.! col) <$> column)

stringExpr :: Parser (MapRow String -> String)
stringExpr = do
    vals <- sepBy rhsValue (symbol "++")
    pure (\row -> concat (vals <*> [row])) <?> "string expression"

numericExpr :: Parser (MapRow String -> Double)
numericExpr = lexeme (makeExprParser numericTerm numericTable) <?> "numeric expression"

numericLiteral :: Parser Double
numericLiteral = try (L.signed spaceConsumer L.float) <|>
    try (L.signed spaceConsumer L.decimal) <?> "numeric literal"

nan :: Double
nan = 0.0/0.0

numericColumn :: Parser (MapRow String -> Double)
numericColumn = (\col row  -> fromMaybe nan (readMaybe (row M.! col))) <$> column <?> "numeric column"

numericTerm  :: Parser (MapRow String -> Double)
numericTerm = parens numericExpr <|> (numericLiteral >>= \x -> pure (const x) ) <|> numericColumn <?> "numeric term"

numericBinaryOperator :: String -> (a -> a -> a) -> Operator Parser a
numericBinaryOperator  name f = InfixL  (f <$ symbol name <?> "numeric operator")

numericOperator :: (Double -> Double -> Double) ->
    (MapRow String -> Double) -> (MapRow String -> Double) -> (MapRow String -> Double)
numericOperator op lhs rhs row = lhs row `op` rhs row

numericTable :: [[Operator Parser (MapRow String -> Double)]]
numericTable = [
          [ numericBinaryOperator  "*"  (numericOperator (*))
          , numericBinaryOperator  "/"  (numericOperator (/))  ]
        , [ numericBinaryOperator  "+"  (numericOperator (+))
          , numericBinaryOperator  "-"  (numericOperator (-))  ] ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer