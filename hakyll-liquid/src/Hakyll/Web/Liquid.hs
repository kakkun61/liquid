{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hakyll.Web.Liquid
  ( parseAndInterpretDefault
  , parseAndInterpret
  , parse
  ) where

import Control.Monad.Catch (MonadThrow (throwM), Exception (displayException))
import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Hakyll hiding (Binary, load)
import qualified Hakyll
import Hakyll.Web.Liquid.Instance ()
import qualified Text.Liquor.Jekyll as Liquid

-- | Parse underlying item and compile it with its metadata as context.
parseAndInterpretDefault :: Compiler (Item String)
parseAndInterpretDefault = getUnderlying >>= getMetadata >>= parseAndInterpret

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
        (Liquid.parse $ Text.pack body)
        maybeLayout
        load
  where
    load :: FilePath -> Compiler (Liquid.Result Liquid.JekyllTemplate, Liquid.Context)
    load filePath = do
      (Item _ body) <- Hakyll.load (fromFilePath filePath)
      pure body

-- | Parse underlying item.
parse :: FilePath -> Compiler (Liquid.Result Liquid.JekyllTemplate, Liquid.Context)
parse filePath = do
  Item identifier body <- getResourceBody
  (Item identifier body) <- Hakyll.load (fromFilePath filePath)
  metadata' <- getMetadata identifier
  pure (Liquid.parse $ Text.pack body, metadata')

instance MonadThrow Compiler where
  throwM = throwError . (:[]) . displayException
