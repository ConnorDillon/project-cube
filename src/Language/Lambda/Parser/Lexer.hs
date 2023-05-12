module Language.Lambda.Parser.Lexer
  ( Parser, lambdaChar, dot, lparen, rparen, chars ) where

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Void ( Void )
import Text.Megaparsec ( some, (<|>), Parsec )
import Text.Megaparsec.Char ( letterChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

lambdaChar :: Parser Text
lambdaChar = symbol "\\" <|> "λ"

dot :: Parser Text
dot = symbol "."

lparen :: Parser Text
lparen = symbol "("

rparen :: Parser Text
rparen = symbol ")"

chars :: Parser Text
chars = lexeme $ Text.pack <$> some letterChar
