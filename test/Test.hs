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
    it "parses lambda expressions" $ do
      "λx.x" `shouldParseTo` "(λx.x)"
      "λx.λy.x y" `shouldParseTo` "(λx.(λy.(x y)))"
      "λx.λy.λz.x y z" `shouldParseTo` "(λx.(λy.(λz.((x y) z))))"
      "λx.x λy.y" `shouldParseTo` "(λx.(x (λy.y)))"
      "(λx.x) λy.y" `shouldParseTo` "((λx.x) (λy.y))"
    it "handles lambda backslash" $ do
      "\\x.x" `shouldParseTo` "(λx.x)"

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
    it "does many step beta reduction" $ do
      "(λx.x) (λy.y) z" `shouldReduceTo` "z"
