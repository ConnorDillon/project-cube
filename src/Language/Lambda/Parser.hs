module Language.Lambda.Parser ( Parser, parse ) where

import Language.Lambda.Term ( Lambda(..) )
import Language.Lambda.Parser.Lexer
  ( Parser, lambdaSym, dot, lparen, rparen, chars, let', bind, in' )

import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Void ( Void )
import Text.Megaparsec
    ( many,
      some,
      between,
      choice,
      ParseErrorBundle,
      (<|>),
      MonadParsec(eof, try) )
import qualified Text.Megaparsec as MP

keywords :: Set.Set Text
keywords = Set.fromList ["let", "in"]

var :: Parser Lambda
var = try $ do
  x <- chars
  if Set.member x keywords
    then fail $ "Keyword: " ++ Text.unpack x
    else return $ Var x

letAbst :: Parser Lambda
letAbst = do
  let'
  name <- chars
  params <- many chars
  bind
  val <- term
  in'
  expr <- term
  return $ App (Abs name expr) $ foldl (.) id (map Abs params) val

abst :: Parser Lambda
abst = do
  lambdaSym
  x <- some chars
  dot
  foldl (.) id (map Abs x) <$> term

notAppl :: Parser Lambda
notAppl = choice
  [ between lparen rparen term
  , letAbst
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
