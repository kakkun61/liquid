{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquor.Interpreter
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

  -- | = Statement
  , S.interpret
  , S.Template
  , S.Statement
  , ShopifyTemplate
  , ShopifyStatement
  , ShopifyStatementSuper

  , S.Plain
  , S.Output
  , S.If
  , S.Case
  , S.For
  , S.Assign

  , S.plain
  , S.output
  , S.if_
  , S.case_
  , S.for
  , S.assign
  ) where

import qualified Text.Liquor.Common as C
import qualified Text.Liquor.Interpreter.Common as C
import qualified Text.Liquor.Interpreter.Expression as E
import qualified Text.Liquor.Interpreter.Statement as S

type ShopifyExpression = E.Expression E.Shopify
type ShopifyStatement = S.Statement E.Shopify S.Shopify
type ShopifyTemplate = [ShopifyStatement]

type ShopifyExpressionSuper e = E.ShopifySuper e
type ShopifyStatementSuper e t = S.ShopifySuper e t
