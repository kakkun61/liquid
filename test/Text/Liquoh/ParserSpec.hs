{-# LANGUAGE OverloadedStrings #-}

module Text.Liquoh.ParserSpec where

import Prelude hiding (Bool, String)
import qualified Prelude
import Test.Hspec
import Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text (parseOnly)
import Data.List.NonEmpty

import Text.Liquoh.Helper ()
import Text.Liquoh.Interpreter.Expression hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Expression as E
import Text.Liquoh.Interpreter.Template hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Template as T
import Text.Liquoh.Parser (expression, template)

type ShopifyExpression = Expression E.Shopify
type ShopifyTemplate = Template (Expression E.Shopify) T.Shopify

spec :: Spec
spec = do
  describe "expression" $ do
    it "1" $ do
      let
        context = object []
        input = "1"
        output = 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "\"hi\"" $ do
      let
        context = object []
        input = "\"hi\""
        output = string "hi" :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "true" $ do
      let
        context = object []
        input = "true"
        output = bool True :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "nil" $ do
      let
        context = object []
        input = "nil"
        output = nil :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "x" $ do
      let
        context = object ["x" .= Aeson.String "x"]
        input = "x"
        output = variable (ObjectKey "x" :| []) :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 == 1" $ do
      let
        context = object []
        input = "1 == 1"
        output = 1 .==. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 != 1" $ do
      let
        context = object []
        input = "1 != 1"
        output = 1 ./=. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 < 1" $ do
      let
        context = object []
        input = "1 < 1"
        output = 1 .<. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 <= 1" $ do
      let
        context = object []
        input = "1 <= 1"
        output = 1 .<=. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 > 1" $ do
      let
        context = object []
        input = "1 > 1"
        output = 1 .>. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 >= 1" $ do
      let
        context = object []
        input = "1 >= 1"
        output = 1 .>=. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 and 1" $ do
      let
        context = object []
        input = "1 and 1"
        output = 1 .&&. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 or 1" $ do
      let
        context = object []
        input = "1 or 1"
        output = 1 .||. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 and 1 < 1" $ do
      let
        context = object []
        input = "1 and 1 < 1"
        output = 1 .&&. 1 .<. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

    it "1 == 1 and 1 < 2 or 1 < 1" $ do
      let
        context = object []
        input = "1 == 1 and 1 < 1"
        output = 1 .==. 1 .&&. 1 .<. 2 .||. 1 .<. 1 :: ShopifyExpression
      evaluate context <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right (evaluate context output)

  describe "template" $ do
    it "hello" $ do
      let
        context = object []
        input = "hello"
        output = [T.plain "hello"] :: [ShopifyTemplate]
      interpret context <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right (interpret context output)

    it "{{ 1 }}" $ do
      let
        context = object []
        input = "{{ 1 }}"
        output = [T.output 1] :: [ShopifyTemplate]
      interpret context <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right (interpret context output)

    it "{% if 1 %}true{% endif %}" $ do
      let
        context = object []
        input = "{% if 1 %}true{% endif %}"
        output =
          [ T.if_
              [ ( number 1 :: ShopifyExpression
                , [ T.plain "true"  :: ShopifyTemplate ]
                )
              ]
          ] :: [ShopifyTemplate]
      interpret context <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right (interpret context output)
