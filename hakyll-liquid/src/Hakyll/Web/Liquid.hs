{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hakyll.Web.Liquid
  ( parseAndInterpretDefault
  , parseAndInterpret
  , load
  ) where

import Control.Monad.Catch (MonadThrow (throwM), Exception (displayException))
import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, Word8)
import qualified Data.Binary as Binary
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hakyll hiding (Binary, load)
import qualified Hakyll
import qualified Text.Liquor.Jekyll as Liquid

-- | Parse underlying item and compile it with its metadata as context.
parseAndInterpretDefault :: Compiler (Item String)
parseAndInterpretDefault = do
  metadata <- getMetadata =<< getUnderlying
  parseAndInterpret metadata

-- | Parse underlying item and compile it with given metadata as context.
parseAndInterpret :: Metadata -> Compiler (Item String)
parseAndInterpret metadata = do
  let
    maybeLayout =
      case HashMap.lookup "layout" metadata of
        Just (Aeson.String layout) -> Just $ Text.unpack layout
        _ -> Nothing
  Item identifier body <- getResourceBody
  Item identifier . Text.unpack
    <$>
      Liquid.loadAndParseAndInterpret'
        metadata
        (toFilePath identifier)
        (Text.pack body)
        maybeLayout
        load'
        (Liquid.parse :: Text -> Liquid.Result Liquid.JekyllTemplate)

load :: Compiler (Item (Text, Liquid.Context))
load = getResourceString >>= splitItem >>= makeItem

load' :: FilePath -> Compiler (Text, Liquid.Context)
load' filePath = Hakyll.load (fromFilePath filePath) >>= splitItem

splitItem :: Item String -> Compiler (Text, Liquid.Context)
splitItem (Item identifier body) = do
  metadata' <- getMetadata identifier
  pure (Text.pack body, metadata')

instance MonadThrow Compiler where
  throwM = throwError . (:[]) . displayException

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

instance Writable Liquid.Context where
  write p = write p . fmap (show . Aeson.Object)
