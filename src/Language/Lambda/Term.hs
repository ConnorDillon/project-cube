module Language.Lambda.Term
  ( Lambda(..)
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
  | Abs Text Lambda
  | App Lambda Lambda
  deriving (Eq, Ord)

instance Show Lambda where
  show l = case l of
    Var x -> Text.unpack x
    Abs x y -> concat ["(λ", Text.unpack x, ".", show y, ")"]
    App x y -> concat ["(", show x, " ", show y, ")"]

alphaConvert :: Text -> Lambda -> Maybe Lambda
alphaConvert new (Abs old term) = Abs new <$> conv old new term
  where 
    conv old new term = case term of
      Var x ->
        if x == new then Nothing
        else if x == old then Just (Var new)
        else Just (Var x)
      Abs x y ->
        if x == new then Nothing
        else if x == old then Just (Abs x y)
        else Abs x <$> conv old new y
      App x y -> App <$> conv old new x <*> conv old new y
alphaConvert _ _ = Nothing

alphaPrime :: Lambda -> Lambda
alphaPrime l@(Abs x y) = case alphaConvert x' l of
  Just n -> n
  Nothing -> alphaPrime $ Abs x' y
  where x' = Text.snoc x '\''
alphaPrime l = l

freeVars :: Lambda -> Set Text
freeVars (Var x) = Set.singleton x
freeVars (Abs x y) = Set.delete x (freeVars y)
freeVars (App x y) = Set.union (freeVars x) (freeVars y)

subs :: Text -> Lambda -> Lambda -> Lambda
subs s r (Var x) = if x == s
  then r
  else Var x
subs s r l@(Abs x y)
  | x == s = l
  | Set.member x (freeVars r) = subs s r (alphaPrime l)
  | otherwise = Abs x $ subs s r y
subs s r (App x y) = App (subs s r x) (subs s r y)
  
betaReduceStep :: Lambda -> Maybe Lambda
betaReduceStep (App (Abs x y) z) = Just $ subs x z y
betaReduceStep (App x@(App _ _) y) = do
  z <- betaReduceStep x
  return $ App z y
betaReduceStep _ = Nothing

betaReduce :: Lambda -> Lambda
betaReduce l = maybe l betaReduce (betaReduceStep l)
