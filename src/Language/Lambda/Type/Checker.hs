module Language.Lambda.Type.Checker (Context, check) where

import Language.Lambda.Term
    ( betaReduce, Lambda(..), Type, AbsType(Pi, La), subs )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as Text

type Context = Map Text Type

check :: Context -> Lambda -> Either Text Type
check c (Var v) = case Map.lookup v c of
  Just t -> return t
  Nothing -> Left $ mappend "Var not found in context:" v
check c (Abs a v t e) = do
  check c t
  c' <- addToCtx v t c
  t' <- check c' e
  return $ case a of
    La -> Abs Pi v t t'
    Pi -> t'
check c (App e1 e2) = do
  t1 <- check c e1
  t2 <- check c e2
  case t1 of
    Abs Pi v t e -> if betaReduce t == betaReduce t2 -- TO-DO: up to alpha equivalence
      then return $ subs v e2 e
      else Left $ mconcat ["Types differ: ", Text.pack $ show t, " ", Text.pack $ show t2]
    _ -> Left $ mappend "Expected a pi type: " $ Text.pack $ show t1

addToCtx :: Text -> Lambda -> Context -> Either Text Context
addToCtx v e c = if Map.member v c
  then Left $ mappend "Var already in context:" v
  else return $ Map.insert v (betaReduce e) c
