{-# LANGUAGE OverloadedStrings #-}

module Text.Liquor.Jekyll.RecursiveSpec where

import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Test.Hspec hiding (context)
import Test.Mockery.Directory

import Text.Liquor.Interpreter
import Text.Liquor.Jekyll.Interpreter (JekyllTemplate)
import Text.Liquor.Jekyll.Parser (parse)
import Text.Liquor.Jekyll.Recursive
import Text.Liquor.Jekyll.Interpreter.Statement

spec :: Spec
spec = do
  describe "aggregate" $ do
    it "foo" $ do
      let
        template = [ plain "foo" ] :: JekyllTemplate
      aggregate template `shouldMatchList` []

    it "{{ foo }}" $ do
      let
        template = [ output (variable $ ObjectKey "foo" :| [] :: ShopifyExpression) ] :: JekyllTemplate
      aggregate template `shouldMatchList` []

    it "{% if true %}foo{% endif %}" $ do
      let
        template =
          [ if_
              [ ( bool True :: ShopifyExpression
                , [ plain "foo" ] :: JekyllTemplate
                )
              ]
          ] :: JekyllTemplate
      aggregate template `shouldMatchList` []

    it "{% case foo %}{% when 1 %}foo{% endcase %}" $ do
      let
        template =
          [ case_
              (variable $ ObjectKey "foo" :| [])
              [ ( number 1, [ plain "foo"] ) ]
          ] :: JekyllTemplate
      aggregate template `shouldMatchList` []

    it "{% for bar in foo %}{{ bar }}{% endfor %}" $ do
      let
        template =
          [ for
              "bar"
              (variable $ ObjectKey "foo" :| [])
              [ output (variable $ ObjectKey "bar" :| [] :: ShopifyExpression) ]
          ] :: [ShopifyStatement]
      aggregate template `shouldMatchList` []

    it "{% assign foo = 1 %}" $ do
      let
        template = [ assign "foo" (number 1) ] :: JekyllTemplate
      aggregate template `shouldMatchList` []

    it "{% include foo %}" $ do
      let
        template = [ include "foo" ] :: JekyllTemplate
      aggregate template `shouldMatchList` ["foo"]

    it "{% include foo %}{% include foo %}" $ do
      let
        template =
          [ include "foo"
          , include "foo"
          ] :: JekyllTemplate
      aggregate template `shouldMatchList` ["foo"]

    it "{% include foo %}{% include bar %}" $ do
      let
        template =
          [ include "foo"
          , include "bar"
          ] :: JekyllTemplate
      aggregate template `shouldMatchList` ["foo", "bar"]

    it "{% if true %}{% include foo %}{% endif %}" $ do
      let
        template =
          [ if_
              [ ( bool True :: ShopifyExpression
                , [ include "foo" ] :: JekyllTemplate
                )
              ]
          ] :: JekyllTemplate
      aggregate template `shouldMatchList` ["foo"]

  describe "loadAndParseAndInterpret" $
    it "The quick brown fox jumps over {% include lazy-dog.txt %}" $ do
      inTempDirectory $ do
        writeFile
          "fox.txt"
          (  "---\n"
          <> "fox: The quick brown fox\n"
          <> "---\n"
          <> "{{ fox }} jumps over {% include lazy-dog.txt %}"
          )
        writeFile "lazy-dog.txt" "the lazy {% include dog.txt %}"
        writeFile "dog.txt" "dog"
        result <- loadAndParseAndInterpret HashMap.empty "fox.txt" load (parse :: Text -> Result JekyllTemplate)
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

  describe "loadAndParseAndInterpret'" $
    it "The quick brown fox jumps over {% include lazy-dog.txt %}" $ do
      inTempDirectory $ do
        let
          filePath = "fox.txt"
        writeFile
          filePath
          (  "---\n"
          <> "fox: The quick brown fox\n"
          <> "---\n"
          <> "{{ fox }} jumps over {% include lazy-dog.txt %}"
          )
        (source, context) <- load filePath
        writeFile "lazy-dog.txt" "the lazy {% include dog.txt %}"
        writeFile "dog.txt" "dog"
        result <- loadAndParseAndInterpret' context filePath source load (parse :: Text -> Result JekyllTemplate)
        result `shouldBe` "The quick brown fox jumps over the lazy dog"
