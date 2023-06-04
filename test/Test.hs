module Main (main) where

import Language.Lambda.Parser
import Language.Lambda.Term
import Language.Lambda.Type.Checker

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Void ( Void )
import Test.Hspec ( hspec, describe, it, shouldBe, context )
import Test.Hspec.Megaparsec ( shouldParse )
import qualified Data.Set as Set
import qualified Data.Map as Map

shouldParseTo :: Text -> Text -> IO ()
shouldParseTo = shouldParse . fmap (Text.pack . show) . parse

shouldResultIn :: (Eq a, Show a) => (Lambda -> a, Text) -> a -> IO ()
shouldResultIn (fn, x) = shouldParse $ fmap fn (parse x)

shouldResultIn' :: (Lambda -> Maybe Lambda, Text) -> Text -> IO ()
shouldResultIn' (fn, x) = shouldResultIn (fmap (Text.pack . show) . fn, x) . Just

shouldReduceTo :: Text -> Text -> IO ()
shouldReduceTo x = shouldParse (fmap (Text.pack . show . betaReduce) (parse x))

checkWith :: Rules -> Context -> Text -> Text -> IO ()
checkWith r c x = shouldParse parser . Right
  where parser = fmap (Text.pack . show) . (check r c) <$> parse x

makeRules x = Rules
  { axioms = Map.singleton "*" "BOX"
  , rules = Set.union (Set.singleton ("*", "*")) (Set.fromList x)
  }

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
      "λx:(T->U->V).x" `shouldParseTo` "(λx:(T->(U->V)).x)"
      "let x:T = y in x" `shouldParseTo` "((λx:T.x) y)"
      "let f:(T -> T) x = x in f y" `shouldParseTo` "((λf:(T->T).(f y)) (λx.x))"
      "let f x:T = x in f y" `shouldParseTo` "((λf.(f y)) (λx:T.x))"
      "let f:(T -> T) x:T = x in f y" `shouldParseTo` "((λf:(T->T).(f y)) (λx:T.x))"
      "λx:(Πx:T.T).x" `shouldParseTo` "(λx:(T->T).x)"
      "λx:Πa:*.Πx:a.a.x" `shouldParseTo` "(λx:(Πa:*.(a->a)).x)"

  describe "interpreter" $ do
    it "does alpha conversion" $ do
      (alphaConvert "y", "λx.x") `shouldResultIn'` "(λy.y)"
      (alphaConvert "y", "λx.λx.x") `shouldResultIn'` "(λy.(λx.x))"
    it "finds free vars" $ do
      (freeVars, "x") `shouldResultIn` Set.singleton "x"
      (freeVars, "λx.x y") `shouldResultIn` Set.singleton "y"
      (freeVars, "λx:a.x") `shouldResultIn` Set.singleton "a"
    it "does one step beta reduction" $ do
      (betaReduceStep, "(λx.x) y") `shouldResultIn'` "y"
      (betaReduceStep, "(λx.x) (λx.x) y") `shouldResultIn'` "((λx.x) y)"
      (betaReduceStep, "(λf.f x) λx.x") `shouldResultIn'` "((λx.x) x)"
    it "does many step beta reduction" $ do
      "x y" `shouldReduceTo` "(x y)"
      "(λx.x) (λy.y) z" `shouldReduceTo` "z"
      "let f x = x in let y = z in let foo = f y in foo" `shouldReduceTo` "z"
    it "does automatic alpha conversion during beta reduction" $ do
      (betaReduceStep, "(λy x.y x) x") `shouldResultIn'` "(λx'.(x x'))"
      "(λy x.y x) x y" `shouldReduceTo` "(x y)"

  describe "type checker" $ do
    context "λ→" $ do
      let ctx = Map.fromList [("T", Var "*"), ("foo", Var "T")]
          shouldCheck = checkWith (makeRules []) ctx
      it "checks vars" $ "foo" `shouldCheck` "T"
      it "checks abstractions" $ "λx:T.x" `shouldCheck` "(T->T)"
      it "checks applications" $ do
        "(λx:T.x) foo" `shouldCheck` "T"
        "(λf:(Πx:T.T) x:T.f x) (λx:T.x) foo" `shouldCheck` "T"
    context "λ2" $ do
      let ctx = Map.fromList [("Int", Var "*")]
          shouldCheck = checkWith (makeRules [("BOX", "*")]) ctx
      it "checks abstractions" $ "λa:*.λx:a.x" `shouldCheck` "(Πa:*.(a->a))"
      it "checks applications" $ "(λa:*.λx:a.x) Int" `shouldCheck` "(Int->Int)"
    context "λω (weak)" $ do
      let shouldCheck = checkWith (makeRules [("BOX", "BOX")]) mempty
      it "checks abstractions" $ "λa:*.a" `shouldCheck` "(*->*)"
      it "checks applications" $ "(λa:Πb:*.*.a) λb:*.b" `shouldCheck` "(*->*)"
    context "λP" $ do
      let ctx = Map.fromList [ ("Foo", Abs Pi "_" (Var "Int") (Var "*"))
                             , ("Int", Var "*")
                             , ("one", Var "Int") ]
          shouldCheck = checkWith (makeRules [("*", "BOX")]) ctx
      it "checks abstractions" $ do
        "λa:Int.Int" `shouldCheck` "(Int->*)"
        "λa:Int.Foo" `shouldCheck` "(Int->(Int->*))"
      it "checks applications" $ do
        "Foo one" `shouldCheck` "*"
        "λa:Int.Foo a" `shouldCheck` "(Int->*)"
