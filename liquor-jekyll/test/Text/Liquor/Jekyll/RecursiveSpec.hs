{-# LANGUAGE OverloadedStrings #-}

module Text.Liquor.Jekyll.RecursiveSpec where

import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import System.Directory
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

  describe "loadAndParseAndInterpret" $ do
    it "α includes β, β includes γ" $ do
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
        result <- loadAndParseAndInterpret HashMap.empty "fox.txt" loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α depends on β as layout, β includes γ" $ do
      inTempDirectory $ do
        writeFile
          "fox.txt"
          (  "---\n"
          <> "layout: dir/wrap.txt\n"
          <> "---\n"
          <> "The quick brown fox"
          )
        createDirectory "dir"
        writeFile "dir/wrap.txt" "{{ content }} jumps over the lazy {% include dir/dog.txt %}"
        writeFile "dir/dog.txt" "dog"
        result <- loadAndParseAndInterpret HashMap.empty "fox.txt" loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α depends on β as layout, β uses α's context" $ do
      inTempDirectory $ do
        writeFile
          "fox.txt"
          (  "---\n"
          <> "layout: dir/wrap.txt\n"
          <> "dog: dog\n"
          <> "---\n"
          <> "The quick brown fox"
          )
        createDirectory "dir"
        writeFile "dir/wrap.txt" "{{ content }} jumps over the lazy {{ dog }}"
        result <- loadAndParseAndInterpret HashMap.empty "fox.txt" loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α includes β , β uses α's context" $ do
      inTempDirectory $ do
        writeFile
          "fox.txt"
          (  "---\n"
          <> "dog: dog\n"
          <> "---\n"
          <> "The quick brown fox{% include jump.txt %}"
          )
        writeFile "jump.txt" " jumps over the lazy {{ dog }}"
        result <- loadAndParseAndInterpret HashMap.empty "fox.txt" loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

  describe "loadAndParseAndInterpret'" $ do
    it "α includes β, β includes γ" $ do
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
        let root = parse source
        writeFile "lazy-dog.txt" "the lazy {% include dog.txt %}"
        writeFile "dog.txt" "dog"
        result <- loadAndParseAndInterpret' context filePath root Nothing loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α depends on β as layout, β includes γ" $ do
      inTempDirectory $ do
        let
          filePath = "fox.txt"
        writeFile
          filePath
          (  "---\n"
          <> "layout: dir/wrap.txt\n"
          <> "---\n"
          <> "The quick brown fox"
          )
        (source, context) <- load filePath
        let root = parse source
        createDirectory "dir"
        writeFile "dir/wrap.txt" "{{ content }} jumps over the lazy {% include dir/dog.txt %}"
        writeFile "dir/dog.txt" "dog"
        result <- loadAndParseAndInterpret' context filePath root (Just "dir/wrap.txt") loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α depends on β as layout, β uses α's context" $ do
      inTempDirectory $ do
        let
          filePath = "fox.txt"
        writeFile
          filePath
          (  "---\n"
          <> "layout: dir/wrap.txt\n"
          <> "dog: dog\n"
          <> "---\n"
          <> "The quick brown fox"
          )
        (source, context) <- load filePath
        let root = parse source
        createDirectory "dir"
        writeFile "dir/wrap.txt" "{{ content }} jumps over the lazy {{ dog }}"
        result <- loadAndParseAndInterpret' context filePath root (Just "dir/wrap.txt") loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

    it "α depends on β as layout, β uses α's context" $ do
      inTempDirectory $ do
        let
          filePath = "fox.txt"
        writeFile
          filePath
          (  "---\n"
          <> "dog: dog\n"
          <> "---\n"
          <> "The quick brown fox{% include jump.txt %}"
          )
        (source, context) <- load filePath
        let root = parse source
        writeFile "jump.txt" " jumps over the lazy {{ dog }}"
        result <- loadAndParseAndInterpret' context filePath root Nothing loadAndParse
        result `shouldBe` "The quick brown fox jumps over the lazy dog"

loadAndParse :: FilePath -> IO (Result JekyllTemplate, Context)
loadAndParse filePath' = do
  (t, c) <- load filePath'
  pure (parse t, c)
