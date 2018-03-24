{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hakyll.Web.Liquid.Instance () where

import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)

import Hakyll

instance (Binary k, Eq k, Hashable k, Binary v) => Binary (HashMap k v) where
  put a = Binary.put $ HashMap.toList a
  get = HashMap.fromList <$> Binary.get

instance (Binary a) => Binary (Vector a) where
  put a = Binary.put $ Vector.toList a
  get = Vector.fromList <$> Binary.get

instance Binary Aeson.Value where
  put (Aeson.Object o) = Binary.put (0 :: Word8) >> Binary.put o
  put (Aeson.Array a) = Binary.put (1 :: Word8) >> Binary.put a
  put (Aeson.String s) = Binary.put (2 :: Word8) >> Binary.put s
  put (Aeson.Number n) = Binary.put (3 :: Word8) >> Binary.put n
  put (Aeson.Bool b) = Binary.put (4 :: Word8) >> Binary.put b
  put Aeson.Null = Binary.put (5 :: Word8)

  get = do
    t <- Binary.get :: Binary.Get Word8
    case t of
      0 -> Aeson.Object <$> Binary.get
      1 -> Aeson.Array <$> Binary.get
      2 -> Aeson.String <$> Binary.get
      3 -> Aeson.Number <$> Binary.get
      4 -> Aeson.Bool <$> Binary.get
      5 -> pure Aeson.Null
      n -> fail $ "unexpected tag: " <> show n

instance (Writable a, Writable b) => Writable (a, b) where
  write p (Item identifier (a, b)) = do
    writeFile p "("
    write p $ Item identifier a
    writeFile p ", "
    write p $ Item identifier b
    writeFile p ")"

instance Writable Text where
  write p = write p . fmap Text.unpack

instance Writable (HashMap Text Aeson.Value) where
  write p = write p . fmap (show . Aeson.Object)
