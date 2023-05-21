module Language.Lambda.Term
  ( Lambda(..)
  , Type(..)
  , alphaConvert
  , alphaPrime
  , freeVars
  , subs
  , betaReduceStep
  , betaReduce
  ) where

import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as Text

data Lambda
  = Var Text
  | Abs Text (Maybe Type) Lambda
  | App Lambda Lambda
  deriving (Eq, Ord)

instance Show Lambda where
  show l = case l of
    Var x -> Text.unpack x
    Abs v t e -> concat
      ["(λ", Text.unpack v, maybe "" ((":" ++) . show) t, ".", show e, ")"] 
    App x y -> concat ["(", show x, " ", show y, ")"]

data Type
  = Type Text
  | Arrow Type Type
  deriving (Eq, Ord)

instance Show Type where
  show t = case t of
    Type x -> Text.unpack x
    Arrow x y -> concat ["(", show x, "->", show y, ")"]

alphaConvert :: Text -> Lambda -> Maybe Lambda
alphaConvert new (Abs old ty term) = Abs new ty <$> conv old new term
  where 
    conv old new term = case term of
      Var x ->
        if x == new then Nothing
        else if x == old then Just (Var new)
        else Just (Var x)
      Abs v t e ->
        if v == new then Nothing
        else if v == old then Just (Abs v t e)
        else Abs v t <$> conv old new e
      App x y -> App <$> conv old new x <*> conv old new y
alphaConvert _ _ = Nothing

alphaPrime :: Lambda -> Lambda
alphaPrime l@(Abs v t e) = case alphaConvert v' l of
  Just x -> x
  Nothing -> alphaPrime $ Abs v' t e
  where v' = Text.snoc v '\''
alphaPrime l = l

freeVars :: Lambda -> Set Text
freeVars (Var x) = Set.singleton x
freeVars (Abs v _ e) = Set.delete v (freeVars e)
freeVars (App x y) = Set.union (freeVars x) (freeVars y)

subs :: Text -> Lambda -> Lambda -> Lambda
subs s r (Var x) = if x == s
  then r
  else Var x
subs s r l@(Abs v t e)
  | v == s = l
  | Set.member v (freeVars r) = subs s r (alphaPrime l)
  | otherwise = Abs v t $ subs s r e
subs s r (App x y) = App (subs s r x) (subs s r y)
  
betaReduceStep :: Lambda -> Maybe Lambda
betaReduceStep (App (Abs v _ e) x) = Just $ subs v x e
betaReduceStep (App x@(App _ _) y) = do
  z <- betaReduceStep x
  return $ App z y
betaReduceStep _ = Nothing

betaReduce :: Lambda -> Lambda
betaReduce l = maybe l betaReduce (betaReduceStep l)
