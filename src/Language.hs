module Language where

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

data Transform = Id
    | Select ColumnSelector
    | Deselect ColumnSelector
    | Head Int
    | Filter (MapRow String -> Bool)
    | Mutate (MapRow String -> MapRow String)

instance Show Transform where
    show Id = "Id()"
    show (Select cs) = "Select("++ show cs ++ ")"
    show (Deselect cs) = "Deselect("++ show cs ++ ")"
    show (Head n) = "Head(" ++ show n ++ ")"
    show (Filter _) = "Filter(<???>)"
    show (Mutate _) = "Mutate(<???>)"

type Recipe = [Transform]
type ColumnSelector = [String]
type Parser = Parsec Void String
type KnownColumnParser = StateT (Set.Set String) Parser

type CSVRow = MapRow String

parseRecipe :: String -> Either String Recipe
parseRecipe s = case (runParser (recipe <* eof) "" s) of
  Left err -> Left $ errorBundlePretty err
  Right fine -> Right fine

recipe :: Parser Recipe
recipe = sepBy transform (symbol "|>")

transform :: Parser Transform
transform = choice [
        Id       <$  (symbol "id"       >> symbol "(" >> symbol ")"),
        Select   <$> (symbol "select"   *> parens columnSelector),
        Deselect <$> (symbol "deselect" *> parens columnSelector),
        Head     <$> (symbol "head"     *> parens (lexeme L.decimal)),
        Filter   <$> (symbol "filter"   *> parens predicates),
        Mutate   <$> (symbol "mutate"   *> parens mutations)
    ] <?> "transform"

specialChars :: [Char]
specialChars = " ,()|+-*/='\"`"

quotedColumn :: Parser String
quotedColumn = between (symbol "`") (symbol "`") (lexeme $ some (noneOf "`"))

column :: Parser String
column = lexeme (some (noneOf specialChars)) <|> quotedColumn <?> "columnName"

columnSelector :: Parser [String]
columnSelector = sepBy column (symbol ",")

predicates :: Parser (MapRow String -> Bool)
predicates = sepBy (try stringPredicate <|> try numericPredicate) (symbol ",") >>=
    \listOfPredicates -> pure (\row -> and $ listOfPredicates <*> [row])

stringPredicate :: Parser (MapRow String -> Bool)
stringPredicate = do
    lhs <- stringExpr
    op <- choice [
        (==) <$ symbol "==",
        (/=) <$ symbol "!="
        ]
    rhs <- stringExpr
    pure (\row -> lhs row `op` rhs row)

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

mutations :: Parser (MapRow String -> MapRow String)
mutations = sepBy (try numericMutation <|> try stringMutation) (symbol ",") >>=
    \listOfMutations -> pure (foldl (.) id listOfMutations)

stringMutation :: Parser (MapRow String -> MapRow String)
stringMutation = (do
    targetColumn <- column
    symbol "="
    rhs <- stringExpr
    pure (\row -> M.insert targetColumn (rhs row) row)) <?> "string mutation"

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