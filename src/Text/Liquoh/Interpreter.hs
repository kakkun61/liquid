{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquoh.Interpreter
  (
  -- | = Common
    (C.:<:)
  , (C.:+:)
  , C.Result
  , C.Context

  -- | = Expression
  , E.evaluate
  , E.Expression
  , ShopifyExpression
  , ShopifyExpressionSuper

  , E.Value
  , E.Variable
  , E.Less
  , E.LessEqual
  , E.Grater
  , E.GraterEqual
  , E.Equal
  , E.NotEqual
  , E.And
  , E.Or
  , E.ArrayAt

  , E.number
  , E.string
  , E.bool
  , E.nil
  , E.array
  , E.variable
  , (E..<.)
  , (E..<=.)
  , (E..>.)
  , (E..>=.)
  , (E..==.)
  , (E../=.)
  , (E..&&.)
  , (E..||.)

  , E.VariablePath
  , E.VariableName (..)

  -- | = Template
  , T.interpret
  , T.Template
  , ShopifyTemplate
  , ShopifyTemplateSuper

  , T.Plain
  , T.Output
  , T.If
  , T.Case
  , T.For
  , T.Assign

  , T.plain
  , T.output
  , T.if_
  , T.case_
  , T.for
  , T.assign
  ) where

import qualified Text.Liquoh.Interpreter.Common as C
import qualified Text.Liquoh.Interpreter.Expression as E
import qualified Text.Liquoh.Interpreter.Template as T

type ShopifyExpression = E.Expression E.Shopify
type ShopifyTemplate = T.Template ShopifyExpression T.Shopify

type ShopifyExpressionSuper e = E.ShopifySuper e
type ShopifyTemplateSuper e t = T.ShopifySuper e t
