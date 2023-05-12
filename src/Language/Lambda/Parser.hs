module Language.Lambda.Parser ( Parser, parse ) where

import Language.Lambda.Term ( Lambda(..) )
import Language.Lambda.Parser.Lexer ( Parser, lambdaChar, dot, lparen, rparen, chars )

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Void ( Void )
import Text.Megaparsec
    ( many,
      between,
      choice,
      ParseErrorBundle,
      (<|>),
      MonadParsec(eof, try) )
import qualified Text.Megaparsec as MP

var :: Parser Lambda
var = fmap Var chars

abst :: Parser Lambda
abst = do
  lambdaChar
  x <- chars
  dot
  Abs x <$> term

notAppl :: Parser Lambda
notAppl = choice
  [ between lparen rparen term
  , abst
  , var
  ]

appl :: Parser Lambda
appl = do
  x <- notAppl
  y <- notAppl
  z <- many notAppl
  return $ foldl App (App x y) z

term :: Parser Lambda
term = try appl <|> notAppl

parse :: Text -> Either (ParseErrorBundle Text Void) Lambda
parse = MP.parse (term <* eof) ""
