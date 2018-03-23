{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Text.Liquor.Jekyll.Interpreter.Statement where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

import Text.Liquor.Interpreter.Common
import Text.Liquor.Interpreter.Expression hiding (Inject, inject, Shopify, ShopifySuper, at)
import Text.Liquor.Interpreter.Statement
import Text.Liquor.Jekyll.Common

type Jekyll = Include :+: Shopify
type JekyllSuper e s = (Include :<: s, ShopifySuper e s)

-- Include

data Include s = Include Text.Text
  deriving Functor

instance Interpret Include where
  interpretAlgebra (Include filepath) c = do
    contents <- evaluateVariable c $ ObjectKey (variableFilePrefix <> (Text.pack $ FilePath.normalise $ Text.unpack filepath)) :| []
    pure (c, render contents)

include :: Include :<: s => Text.Text -> Statement e s
include = inject . Include
