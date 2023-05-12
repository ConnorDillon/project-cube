module Language.Lambda.Term ( Lambda(..) ) where

import Data.Text ( Text )
import qualified Data.Text as Text

data Lambda
  = Var Text
  | Abs Text Lambda
  | App Lambda Lambda
  deriving Eq

instance Show Lambda where
  show l = case l of
    Var x -> Text.unpack x
    Abs x y -> concat ["(λ", Text.unpack x, ".", show y, ")"]
    App x y -> concat ["(", show x, " ", show y, ")"]
