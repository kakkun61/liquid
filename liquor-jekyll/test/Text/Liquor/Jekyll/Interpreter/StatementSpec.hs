{-# LANGUAGE OverloadedStrings #-}

module Text.Liquor.Jekyll.Interpreter.StatementSpec where

import Prelude hiding (Bool, String)
import Test.Hspec
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import qualified Data.Vector as Vector

import Text.Liquor.Interpreter (ShopifyExpression)
import Text.Liquor.Interpreter.Expression hiding (Shopify)
import qualified Text.Liquor.Interpreter.Expression as E
import Text.Liquor.Interpreter.Statement hiding (Shopify)
import Text.Liquor.Jekyll.Common
import Text.Liquor.Jekyll.Interpreter (JekyllStatement)
import Text.Liquor.Jekyll.Interpreter.Statement hiding (Jekyll)

spec :: Spec
spec = do
  describe "simple" $ do
    it "{% include foo %}" $ do
      let
        context = HashMap.fromList [(variableFilePrefix <> "foo", Aeson.String "bar")]
        template =
          [ include "foo"
          ] :: [JekyllStatement]
      interpret' context template `shouldBe` Right (context, "bar")
