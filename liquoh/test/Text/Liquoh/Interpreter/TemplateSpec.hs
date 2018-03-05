{-# LANGUAGE OverloadedStrings #-}

module Text.Liquoh.Interpreter.TemplateSpec where

import Prelude hiding (Bool, String)
import Test.Hspec
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Vector as Vector

import Text.Liquoh.Interpreter (ShopifyExpression, ShopifyTemplate)
import Text.Liquoh.Interpreter.Expression hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Expression as E
import Text.Liquoh.Interpreter.Template hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Template as T

spec :: Spec
spec = do
  describe "simple" $ do
    it "{% if true %}foo{% endif %}" $ do
      let
        context = object []
        template =
          [ if_
              [ ( bool True :: ShopifyExpression
                , [ plain "foo" :: ShopifyTemplate ]
                )
              ]
          ] :: [ShopifyTemplate]
      interpret' context template `shouldBe` Right (context, "foo")

    it "{% for bar in foo %}{{ bar }}{% endfor %}" $ do
      let
        context = object ["foo" .= Aeson.Array (Vector.fromList $ Aeson.Number <$> [1, 2, 3])]
        template =
          [ for
              "bar"
              (variable $ ObjectKey "foo" :| [])
              [ output (variable $ ObjectKey "bar" :| [] :: ShopifyExpression) ]
          ] :: [ShopifyTemplate]
        context' = unsafeAttachContext context "bar" $ Number 3
      interpret' context template `shouldBe` Right (context', "123")

  describe "complex" $ do
    it "<html><body>{% if true %}true{% else %}false{% endif %}</body></html>" $ do
      let
        context = object []
        template =
          [ plain "<html><body>"
          , if_
              [ ( bool True :: ShopifyExpression
                , [ plain "true" :: ShopifyTemplate ]
                )
              , ( bool True :: ShopifyExpression
                , [ plain "false" :: ShopifyTemplate ]
                )
              ]
          , plain "</body></html>"
          ] :: [ShopifyTemplate]
      interpret' context template `shouldBe` Right (context, "<html><body>true</body></html>")

    it "<html>\\n<body>\\n{% if true %}\\n  true\\n{% else %}\\n  false\\n{% endif %}\\n</body>\\n</html>\\n" $ do
      let
        context = object []
        template =
          [ plain "<html>\n<body>\n"
          , if_
              [ ( bool True :: ShopifyExpression
                , [ plain "\n  true\n" :: ShopifyTemplate ]
                )
              , ( bool True :: ShopifyExpression
                , [ plain "\n  false\n" :: ShopifyTemplate ]
                )
              ]
          , plain "\n</body>\n</html>\n"
          ] :: [ShopifyTemplate]
      interpret' context template `shouldBe` Right (context, "<html>\n<body>\n\n  true\n\n</body>\n</html>\n")
