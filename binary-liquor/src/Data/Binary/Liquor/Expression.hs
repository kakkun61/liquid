{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Liquor.Expression () where

import Prelude hiding (String)
import Data.Binary (Binary, Get, get, put)
import Data.Vector.Binary ()
import Data.Word (Word8)

import Text.Liquor.Interpreter.Expression

instance Binary (e (Expression e)) => Binary (Expression e) where
  put (Inject v) = put v
  get = Inject <$> get

instance Binary ValueData where
  put (Number v) = put (0 :: Word8) >> put v
  put (String v) = put (2 :: Word8) >> put v
  put (Bool v)   = put (1 :: Word8) >> put v
  put Nil        = put (3 :: Word8)
  put (Array v)  = put (4 :: Word8) >> put v

  get = do
    t <- get :: Get Word8
    case t of
      0 -> Number <$> get
      1 -> String <$> get
      2 -> Bool <$> get
      3 -> pure Nil
      4 -> Array <$> get
      _ -> error "unexpected"

instance Binary (Variable e) where
  put (Variable v) = put v
  get = Variable <$> get

instance Binary VariableName where
  put (ObjectKey v) = put (0 :: Word8) >> put v
  put (ArrayKey v)  = put (1 :: Word8) >> put v

  get = do
    t <- get :: Get Word8
    case t of
      0 -> ObjectKey <$> get
      1 -> ArrayKey <$> get
      _ -> error "unexpected"

instance Binary e => Binary (Less e) where
  put (Less v u) = put v >> put u
  get = Less <$> get <*> get

instance Binary e => Binary (LessEqual e) where
  put (LessEqual v u) = put v >> put u
  get = LessEqual <$> get <*> get

instance Binary e => Binary (Greater e) where
  put (Greater v u) = put v >> put u
  get = Greater <$> get <*> get

instance Binary e => Binary (GreaterEqual e) where
  put (GreaterEqual v u) = put v >> put u
  get = GreaterEqual <$> get <*> get

instance Binary e => Binary (Equal e) where
  put (Equal v u) = put v >> put u
  get = Equal <$> get <*> get

instance Binary e => Binary (NotEqual e) where
  put (NotEqual v u) = put v >> put u
  get = NotEqual <$> get <*> get

instance Binary e => Binary (And e) where
  put (And v u) = put v >> put u
  get = And <$> get <*> get

instance Binary e => Binary (Or e) where
  put (Or v u) = put v >> put u
  get = Or <$> get <*> get

instance Binary e => Binary (ArrayAt e) where
  put (ArrayAt v u) = put v >> put u
  get = ArrayAt <$> get <*> get
