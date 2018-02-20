{-# LANGUAGE OverloadedStrings #-}

module Text.Liquoh.Interpreter.TemplateSpec where

import Prelude hiding (Bool, String)
import Test.Hspec
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Vector as Vector

import Text.Liquoh.Interpreter.Expression hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Expression as E
import Text.Liquoh.Interpreter.Template hiding (Shopify)
import qualified Text.Liquoh.Interpreter.Template as T

spec :: Spec
spec = do
  describe "if" $
    it "if true" $ do
      let
        context = object []
        template =
          [ if_
              [ ( bool True :: Expression E.Shopify
                , [ plain "foo" :: Template (Expression E.Shopify) T.Shopify ]
                )
              ] :: Template (Expression E.Shopify) T.Shopify
          ]
      interpret' context template `shouldBe` Right (context, "foo")

  describe "for" $
    it "for x in xs" $ do
      let
        context = object ["foo" .= Aeson.Array (Vector.fromList $ Aeson.Number <$> [1, 2, 3])]
        template =
          [ for
              "bar"
              (variable $ ObjectKey "foo" :| [])
              [ output (variable $ ObjectKey "bar" :| [] :: Expression E.Shopify) ]
              :: Template (Expression E.Shopify) T.Shopify
          ]
        context' = unsafeAttachContext context "bar" $ Number 3
      interpret' context template `shouldBe` Right (context', "123")
