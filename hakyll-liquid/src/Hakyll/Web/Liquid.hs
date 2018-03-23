{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hakyll.Web.Liquid
  ( parseAndInterpretDefault
  , parseAndInterpret
  , load
  ) where

import Control.Monad.Catch (MonadThrow (throwM), Exception (displayException))
import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
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
        load
        (Liquid.parse :: Text -> Liquid.Result Liquid.JekyllTemplate)

load :: FilePath -> Compiler (Text, Liquid.Context)
load filePath = do
  (Item identifier body) <- Hakyll.load (fromFilePath filePath)
  metadata' <- getMetadata identifier
  pure (Text.pack body, metadata')

instance MonadThrow Compiler where
  throwM = throwError . (:[]) . displayException
