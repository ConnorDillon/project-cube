module Main (main) where

import Language.Lambda.Parser ( parse )
import Language.Lambda.Term
  ( Lambda(..), alphaConvert, freeVars, betaReduceStep, betaReduce )

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Void ( Void )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.Megaparsec ( shouldParse )
import qualified Data.Set as Set

shouldParseTo :: Text -> Text -> IO ()
shouldParseTo = shouldParse . fmap (Text.pack . show) . parse

shouldResultIn :: (Eq a,Show a) => (Lambda -> a, Text) -> a -> IO ()
shouldResultIn (fn, x) = shouldParse $ fmap fn (parse x)

shouldResultIn' :: (Lambda -> Maybe Lambda, Text) -> Text -> IO ()
shouldResultIn' (fn, x) = shouldResultIn (fmap (Text.pack . show) . fn, x) . Just

shouldReduceTo :: Text -> Text -> IO ()
shouldReduceTo x = shouldParse (fmap (Text.pack . show . betaReduce) (parse x))

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "parses vars" $ do
      "x" `shouldParseTo` "x"
    it "parses applications" $ do
      "x y" `shouldParseTo` "(x y)"
      "x y z" `shouldParseTo` "((x y) z)"
    it "parses abstractions" $ do
      "λx.x" `shouldParseTo` "(λx.x)"
      "λx y.x" `shouldParseTo` "(λx.(λy.x))"
    it "handles lambda backslash" $ do
      "\\x.x" `shouldParseTo` "(λx.x)"
    it "parses lambda expressions" $ do
      "λx.λy.x y" `shouldParseTo` "(λx.(λy.(x y)))"
      "λx y z.x y z" `shouldParseTo` "(λx.(λy.(λz.((x y) z))))"
      "λx.x λy.y" `shouldParseTo` "(λx.(x (λy.y)))"
      "(λx.x) λy.y" `shouldParseTo` "((λx.x) (λy.y))"
    it "parses let expressions" $ do
      "let x = y in z" `shouldParseTo` "((λx.z) y)"
      "let f = λx.x in f x" `shouldParseTo` "((λf.(f x)) (λx.x))"
      "let f x y = x y in f x" `shouldParseTo` "((λf.(f x)) (λx.(λy.(x y))))"
      "let x = y in let a = b in c" `shouldParseTo` "((λx.((λa.c) b)) y)"
      "let f x = x in let y = z in f y" `shouldParseTo` "((λf.((λy.(f y)) z)) (λx.x))"
    it "parses typed expressions" $ do
      "λx:T.x" `shouldParseTo` "(λx:T.x)"
      "λf:(T->T) x:T.f x" `shouldParseTo` "(λf:(T->T).(λx:T.(f x)))"
      "λx:T->U->V.x" `shouldParseTo` "(λx:(T->(U->V)).x)"
      "let x:T = y in x" `shouldParseTo` "((λx:T.x) y)"
      "let f:(T -> T) x = x in f y" `shouldParseTo` "((λf:(T->T).(f y)) (λx.x))"
      "let f x:T = x in f y" `shouldParseTo` "((λf.(f y)) (λx:T.x))"
      "let f:(T -> T) x:T = x in f y" `shouldParseTo` "((λf:(T->T).(f y)) (λx:T.x))"

  describe "interpreter" $ do
    it "does alpha conversion" $ do
      (alphaConvert "y", "λx.x") `shouldResultIn'` "(λy.y)"
      (alphaConvert "y", "λx.λx.x") `shouldResultIn'` "(λy.(λx.x))"
    it "finds free vars" $ do
      (freeVars, "x") `shouldResultIn` Set.singleton "x"
      (freeVars, "λx.x y") `shouldResultIn` Set.singleton "y"
    it "does one step beta reduction" $ do
      (betaReduceStep, "(λx.x) y") `shouldResultIn'` "y"
      (betaReduceStep, "(λx.x) (λx.x) y") `shouldResultIn'` "((λx.x) y)"
      (betaReduceStep, "(λf.f x) λx.x") `shouldResultIn'` "((λx.x) x)"
    it "does many step beta reduction" $ do
      "x y" `shouldReduceTo` "(x y)"
      "(λx.x) (λy.y) z" `shouldReduceTo` "z"
      "let f x = x in let y = z in f y" `shouldReduceTo` "z"
    it "does automatic alpha conversion during beta reduction" $ do
      (betaReduceStep, "(λy x.y x) x") `shouldResultIn'` "(λx'.(x x'))"
      "(λy x.y x) x y" `shouldReduceTo` "(x y)"
