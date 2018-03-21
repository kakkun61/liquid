{-# LANGUAGE OverloadedStrings #-}

module Text.Liquor.Interpreter.StatementSpec where

import Prelude hiding (Bool, String)
import Test.Hspec hiding (context)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Vector as Vector

import Text.Liquor.Interpreter (ShopifyExpression, ShopifyStatement)
import Text.Liquor.Interpreter.Expression hiding (Shopify)
import Text.Liquor.Interpreter.Statement hiding (Shopify)

spec :: Spec
spec = do
  describe "simple" $ do
    it "{% if true %}foo{% endif %}" $ do
      let
        context = HashMap.empty
        template =
          [ if_
              [ ( bool True :: ShopifyExpression
                , [ plain "foo" :: ShopifyStatement ]
                )
              ]
          ] :: [ShopifyStatement]
      interpret' context template `shouldBe` Right (context, "foo")

    it "{% for bar in foo %}{{ bar }}{% endfor %}" $ do
      let
        context = HashMap.fromList [("foo", Aeson.Array (Vector.fromList $ Aeson.Number <$> [1, 2, 3]))]
        template =
          [ for
              "bar"
              (variable $ ObjectKey "foo" :| [])
              [ output (variable $ ObjectKey "bar" :| [] :: ShopifyExpression) ]
          ] :: [ShopifyStatement]
        context' = unsafeAttachContext context "bar" $ Number 3
      interpret' context template `shouldBe` Right (context', "123")

  describe "complex" $ do
    it "<html><body>{% if true %}true{% else %}false{% endif %}</body></html>" $ do
      let
        context = HashMap.empty
        template =
          [ plain "<html><body>"
          , if_
              [ ( bool True :: ShopifyExpression
                , [ plain "true" :: ShopifyStatement ]
                )
              , ( bool True :: ShopifyExpression
                , [ plain "false" :: ShopifyStatement ]
                )
              ]
          , plain "</body></html>"
          ] :: [ShopifyStatement]
      interpret' context template `shouldBe` Right (context, "<html><body>true</body></html>")

    it "<html>\\n<body>\\n{% if true %}\\n  true\\n{% else %}\\n  false\\n{% endif %}\\n</body>\\n</html>\\n" $ do
      let
        context = HashMap.empty
        template =
          [ plain "<html>\n<body>\n"
          , if_
              [ ( bool True :: ShopifyExpression
                , [ plain "\n  true\n" :: ShopifyStatement ]
                )
              , ( bool True :: ShopifyExpression
                , [ plain "\n  false\n" :: ShopifyStatement ]
                )
              ]
          , plain "\n</body>\n</html>\n"
          ] :: [ShopifyStatement]
      interpret' context template `shouldBe` Right (context, "<html>\n<body>\n\n  true\n\n</body>\n</html>\n")
