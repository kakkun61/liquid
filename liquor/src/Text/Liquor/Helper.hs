{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Text.Liquor.Helper where

import Prelude hiding (String)
import Data.String

import Text.Liquor.Interpreter

instance Value :<: f => IsString (Expression f) where
  fromString = string . fromString

instance Value :<: f => Num (Expression f) where
  fromInteger = number . fromInteger
