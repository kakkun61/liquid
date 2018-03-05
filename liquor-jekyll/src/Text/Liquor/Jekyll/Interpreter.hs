{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquor.Jekyll.Interpreter
  ( JekyllTemplate
  , JekyllStatement
  , JekyllStatementSuper

  , S.Include

  , S.include
  ) where

import qualified Text.Liquor.Interpreter.Expression as SE
import qualified Text.Liquor.Interpreter.Statement as SS
import qualified Text.Liquor.Jekyll.Interpreter.Statement as S

type JekyllStatement = SS.Statement SE.Shopify S.Jekyll
type JekyllTemplate = [JekyllStatement]

type JekyllStatementSuper e t = S.JekyllSuper e t
