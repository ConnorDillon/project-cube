module Language.Lambda.Parser ( Parser, parse ) where

import Language.Lambda.Term ( Lambda(..), Type, AbsType(..) )
import Language.Lambda.Parser.Lexer
    ( Parser,
      lambdaSym,
      piSym,
      colon,
      arrow,
      star,
      dot,
      lparen,
      rparen,
      let',
      bind,
      in',
      chars )

import Data.Maybe (fromMaybe)
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Void ( Void )
import Text.Megaparsec
    ( many,
      some,
      optional,
      between,
      choice,
      ParseErrorBundle,
      (<|>),
      MonadParsec(eof, try) )
import qualified Text.Megaparsec as MP
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixR))
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))

keywords :: Set.Set Text
keywords = Set.fromList ["let", "in"]

var :: Parser Lambda
var = try $ do
  x <- chars <|> star
  if Set.member x keywords
    then fail $ "Keyword: " ++ Text.unpack x
    else return $ Var x

binding :: Parser (Text, Type)
binding = do
  x <- chars
  y <- optional $ colon >> notAppl
  return (x, fromMaybe (Var "_") y)

foldAbst :: AbsType -> [(Text, Type)] -> Lambda -> Lambda
foldAbst a bindings = foldl (.) id (map (uncurry $ Abs a) bindings)

letAbst :: Parser Lambda
letAbst = do
  let'
  (name, ty) <- binding
  bindings <- many binding
  bind
  val <- expr
  in'
  expr <- expr
  return $ App (Abs La name ty expr) $ foldAbst La bindings val

absType :: Parser AbsType
absType = La <$ lambdaSym <|> Pi <$ piSym
  
abst :: Parser Lambda
abst = do
  a <- absType
  bindings <- some binding
  dot
  foldAbst a bindings <$> expr

notAppl :: Parser Lambda
notAppl = choice
  [ between lparen rparen expr
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

expr :: Parser Lambda
expr = makeExprParser term [[ InfixR $ Abs Pi "_" <$ arrow ]]

parse :: Text -> Either (ParseErrorBundle Text Void) Lambda
parse = MP.parse (expr <* eof) ""
