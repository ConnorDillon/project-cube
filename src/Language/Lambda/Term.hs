module Language.Lambda.Term
  ( Lambda(..)
  , AbsType(..)
  , Type
  , alphaConvert
  , alphaPrime
  , freeVars
  , subs
  , betaReduce
  , betaNormalForm
  , headNormalForm
  ) where

import Data.Maybe (fromMaybe)
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as Text

data Lambda
  = Var Text
  | Abs AbsType Text Type Lambda
  | App Lambda Lambda
  deriving Ord

instance Show Lambda where
  show l = case l of
    Var x -> Text.unpack x
    Abs a v t e -> case a of
      La -> abst
      Pi -> if Set.member v (freeVars e)
              then abst
              else concat ["(", show t, "->", show e, ")"]
      where
        type' = let t' = show t in if t' == "_" then "" else ":" ++ t'
        abst = concat ["(" , show a, Text.unpack v, type', ".", show e, ")"] 
    App x y -> concat ["(", show x, " ", show y, ")"]

instance Eq Lambda where
  (Var x) == (Var x') = x == x'
  (App x y) == (App x' y') = x == x' && y == y'
  x@(Abs a v t e) == y@(Abs a' v' t' e') = if v == v'
    then a == a' && t == t' && e == e'
    else case alphaConvert v y of
      Just x' -> x == x'
      Nothing -> alphaPrime x == y

data AbsType = La | Pi
  deriving (Eq, Ord)

instance Show AbsType where
  show La = "λ"
  show Pi = "Π"

type Type = Lambda

alphaConvert :: Text -> Lambda -> Maybe Lambda
alphaConvert new (Abs a old ty term) = Abs a new ty <$> conv old new term
  where 
    conv old new term = case term of
      Var x ->
        if x == new then Nothing
        else if x == old then Just (Var new)
        else Just (Var x)
      Abs a v t e ->
        let t' = fromMaybe t (conv old new t) in
        if v == new then Nothing
        else if v == old then Just (Abs a v t' e)
        else Abs a v t' <$> conv old new e
      App x y -> App <$> conv old new x <*> conv old new y
alphaConvert _ _ = Nothing

alphaPrime :: Lambda -> Lambda
alphaPrime l@(Abs a v t e) = case alphaConvert v' l of
  Just x -> x
  Nothing -> alphaPrime $ Abs a v' t e
  where v' = Text.snoc v '\''
alphaPrime l = l

freeVars :: Lambda -> Set Text
freeVars (Var x) = Set.singleton x
freeVars (Abs a v t e) = Set.delete v $ Set.union
  (if t == Var "_" then mempty else freeVars t)
  (freeVars e)
freeVars (App x y) = Set.union (freeVars x) (freeVars y)

subs :: Text -> Lambda -> Lambda -> Lambda
subs s r (Var x) = if x == s
  then r
  else Var x
subs s r l@(Abs a v t e)
  | v == s = l
  | Set.member v (freeVars r) = subs s r (alphaPrime l)
  | otherwise = Abs a v (subs s r t) (subs s r e)
subs s r (App x y) = App (subs s r x) (subs s r y)
  
betaReduce :: Lambda -> Maybe Lambda
betaReduce (App (Abs La v _ e) x) = Just $ subs v x e
betaReduce (App x@(App _ _) y) = do
  z <- betaReduce x
  return $ App z y
betaReduce _ = Nothing

betaNormalForm :: Lambda -> Lambda
betaNormalForm e = case betaReduce e of
  Just e' -> betaNormalForm e'
  Nothing -> case e of
    App e1 e2 -> App (betaNormalForm e1) (betaNormalForm e2)
    Abs a v t e -> Abs a v (betaNormalForm t) (betaNormalForm e)
    _ -> e

headNormalForm :: Lambda -> Lambda
headNormalForm e = maybe e headNormalForm (betaReduce e)
