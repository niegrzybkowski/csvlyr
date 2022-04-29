module Language where

import Data.Void ( Void )
import Text.Megaparsec
    ( (<?>), noneOf, choice, sepBy, some, Parsec, parseTest, MonadParsec (eof), parse, errorBundlePretty, parseErrorPretty, runParser )
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

data Transform = Id
    | Select ColumnSelector
    | Deselect ColumnSelector
    | Head Int
    | Drop Int
    deriving Show

type Recipe = [Transform]

parseRecipe :: String -> Either String Recipe
parseRecipe s = case runParser (recipe <* eof) "" s of
  Left err -> Left $ errorBundlePretty err
  Right fine -> Right fine

type ColumnSelector = [String]
type Parser = Parsec Void String

specialChars :: [Char]
specialChars = " ,()|"

columnSelector :: Parser [String]
columnSelector = sepBy (lexeme $ some (noneOf specialChars)) (symbol ",")

wrapInParen :: Parser a -> Parser a
wrapInParen parser = symbol "(" *> parser <* symbol ")"

transform :: Parser Transform
transform = choice [
        Id       <$  (symbol "id" >> symbol "(" >> symbol ")"),
        Select   <$> (symbol "select" *> wrapInParen columnSelector),
        Deselect <$> (symbol "deselect" *> wrapInParen columnSelector),
        Head     <$> (symbol "head" *> wrapInParen (lexeme L.decimal)),
        Drop     <$> (symbol "drop" *> wrapInParen (lexeme L.decimal))
    ] <?> "transform"

recipe :: Parser Recipe
recipe = sepBy transform (symbol "|>")

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

