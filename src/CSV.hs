{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module CSV where

import Data.Void ( Void ) 
import Text.Megaparsec
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as L
type Parser = Parsec Void String

data Value = StringValue String -- | IntValue Int | DoubleValue Double

