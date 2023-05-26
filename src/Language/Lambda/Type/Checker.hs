module Language.Lambda.Type.Checker (Context, typed, check) where

import Language.Lambda.Term

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as Text

type Context = Map Text Type

typed :: Lambda OptType -> Maybe (Lambda Type)
typed = traverse getType

check :: Context -> Lambda Type -> Maybe Type
check c (Var v) = Map.lookup v c
check c (Abs v t l') = Arrow t <$> check (Map.insert v t c) l'
check c (App l1 l2) = do
  l1t <- check c l1
  l2t <- check c l2
  case l1t of
    Arrow t1 t2 -> if t1 == l2t
      then Just t2
      else Nothing
    _ -> Nothing
