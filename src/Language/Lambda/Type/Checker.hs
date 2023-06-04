module Language.Lambda.Type.Checker (Rules(..), Context, check) where

import Language.Lambda.Term
    ( betaReduce, Lambda(..), Type, AbsType(Pi, La), subs )

import Control.Monad ((>=>))
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Bifunctor (Bifunctor(bimap))

data Rules = Rules
  { axioms :: Map Text Text
  , rules :: Set (Text, Text)
  }

type Context = Map Text Type

check :: Rules -> Context -> Lambda -> Either Text Type
check rls ctx = infer (mappend ctx (Var <$> axioms rls))
  where
    sorts = Set.fromList $ concatMap (\(x, y) -> [Var x, Var y]) $ Map.toList $ axioms rls

    checkSort c t = if Set.member t sorts
      then return ()
      else Left $ mappend "Is not a sort: " $ Text.pack $ show t

    checkRule s1 s2 = if Set.member (s1, s2) $ Set.map (bimap Var Var) $ rules rls
      then return ()
      else Left $ mappend "Rule not supported: " $ Text.pack $ show (s1, s2)

    addToCtx c v e = if Map.member v c
      then Left $ mappend "Var already in context:" v
      else return $ Map.insert v (betaReduce e) c

    infer c e = case e of
      Var v -> case Map.lookup v c of
        Just t -> return t
        Nothing -> Left $ mappend "Var not found in context:" v
      Abs a v t e -> do
        argT <- infer c t
        checkSort c argT
        retT <- addToCtx c v t >>= flip infer e
        case a of
          La -> do
            let pi = Abs Pi v t retT
            s <- infer c pi
            checkSort c s
            return pi
          Pi -> do
            checkRule argT retT
            return retT
      App e1 e2 -> do
        t1 <- infer c e1
        t2 <- infer c e2
        infer c t2 >>= checkSort c
        case t1 of
          -- TO-DO: beta equivalence: to beta normal form and alpha equivalence
          Abs Pi v t e -> if betaReduce t == betaReduce t2
            then return $ subs v e2 e
            else Left $ mconcat ["Types differ: ", Text.pack $ show t, " ", Text.pack $ show t2]
          _ -> Left $ mappend "Expected a pi type: " $ Text.pack $ show t1
