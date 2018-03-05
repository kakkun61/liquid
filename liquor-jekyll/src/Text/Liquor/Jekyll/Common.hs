{-# LANGUAGE OverloadedStrings #-}

module Text.Liquor.Jekyll.Common where

import Control.Exception
import Data.Text (Text)

variableFilePrefix :: Text
variableFilePrefix = "*"

newtype LiquidJekyllException = LiquidJekyllException Text
  deriving (Eq, Show)

instance Exception LiquidJekyllException
