module Main (main) where

import Language.Lambda ( parse )

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Void ( Void )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.Megaparsec ( shouldParse )
import Text.Megaparsec ( ParseErrorBundle )

shouldParseTo :: Text -> Text -> IO ()
shouldParseTo x = (fmap (Text.pack . show) (parse x) `shouldParse`)

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
