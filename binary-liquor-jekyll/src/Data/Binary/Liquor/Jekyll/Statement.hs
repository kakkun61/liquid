{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Liquor.Jekyll.Statement () where

import Data.Binary (Binary, get, put)

import Text.Liquor.Jekyll.Interpreter.Statement

instance Binary (Include s) where
  put (Include v) = put v
  get = Include <$> get
