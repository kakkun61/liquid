{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquor.Jekyll.Parser where

import Data.Attoparsec.Text
import qualified Data.Text as Text

import Text.Liquor.Interpreter ((:<:))
import qualified Text.Liquor.Interpreter as SI
import qualified Text.Liquor.Jekyll.Interpreter as I
import Text.Liquor.Parser hiding (statement, template, parse)

include :: I.Include :<: s => Parser (SI.Statement e s)
include = I.include <$> (tagWith "include" (Text.pack <$> manyTill1 anyChar space))

statement :: (I.JekyllStatementSuper e s, SI.ShopifyExpressionSuper e) => Parser (SI.Statement e s) -> Parser (SI.Statement e s)
statement stmt =
  choice
    [ output
    , if_ stmt
    , for stmt
    , include
    ] <?> "Block Parsing"

template :: (I.JekyllStatementSuper e s, SI.ShopifyExpressionSuper e) => Parser (SI.Template e s)
template =
  let stmt = statement stmt
  in template' stmt

parse :: (I.JekyllStatementSuper e s, SI.ShopifyExpressionSuper e) => Text.Text -> SI.Result (SI.Template e s)
parse = parse' template
