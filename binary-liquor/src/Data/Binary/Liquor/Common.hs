{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Liquor.Common () where

import Prelude hiding (String)
import Data.Binary (Binary, Get, get, put)
import Data.Vector.Binary ()
import Data.Word (Word8)

import Text.Liquor.Interpreter.Common

instance (Binary (f e), Binary (g e)) => Binary ((f :+: g) e) where
  put (InjectLeft v) = put (0 :: Word8) >> put v
  put (InjectRight v) = put (1 :: Word8) >> put v
  get = do
    t <- get :: Get Word8
    case t of
      0 -> InjectLeft <$> get
      1 -> InjectRight <$> get
      _ -> error "unexpected"
