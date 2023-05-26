module Language.Lambda.Parser ( Parser, OptType(..), parse ) where

import Language.Lambda.Term ( Lambda(..), Type(..) )
import Language.Lambda.Parser.Lexer

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

newtype OptType = OptType { getType :: Maybe Type }
  deriving (Eq, Ord)

instance Show OptType where
  show = maybe "" show . getType

keywords :: Set.Set Text
keywords = Set.fromList ["let", "in"]

var :: Parser (Lambda a)
var = try $ do
  x <- chars
  if Set.member x keywords
    then fail $ "Keyword: " ++ Text.unpack x
    else return $ Var x

type' :: Parser Type
type' = between lparen rparen type'
  <|> arr
  <|> fmap Type chars
  where
    arr = try $ do
      x <- chars
      arrow
      Arrow (Type x) <$> type'

binding :: Parser (Text, OptType)
binding = do
  x <- chars
  y <- optional $ colon >> type'
  return (x, OptType y)

foldAbst :: [(Text, OptType)] -> Lambda OptType -> Lambda OptType
foldAbst bindings = foldl (.) id (map (uncurry Abs) bindings)

letAbst :: Parser (Lambda OptType)
letAbst = do
  let'
  (name, ty) <- binding
  bindings <- many binding
  bind
  val <- term
  in'
  expr <- term
  return $ App (Abs name ty expr) $ foldAbst bindings val

abst :: Parser (Lambda OptType)
abst = do
  lambdaSym
  bindings <- some binding
  dot
  foldAbst bindings <$> term

notAppl :: Parser (Lambda OptType)
notAppl = choice
  [ between lparen rparen term
  , letAbst
  , abst
  , var
  ]

appl :: Parser (Lambda OptType)
appl = do
  x <- notAppl
  y <- notAppl
  z <- many notAppl
  return $ foldl App (App x y) z

term :: Parser (Lambda OptType)
term = try appl <|> notAppl

parse :: Text -> Either (ParseErrorBundle Text Void) (Lambda OptType)
parse = MP.parse (term <* eof) ""
