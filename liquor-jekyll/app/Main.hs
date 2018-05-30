{-# LANGUAGE OverloadedStrings #-}

import Text.Liquor.Jekyll

import qualified Codec.Binary.UTF8.String as UTF8Codec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  text <-
    case args of
      [filePath] -> go HashMap.empty filePath
      ["", filePath] -> go HashMap.empty filePath
      [ctxStr, filePath] ->
        case Aeson.decode (ByteString.pack $ UTF8Codec.encode ctxStr) of
          Just (Aeson.Object context) -> go context filePath
          Just _ -> error "top level of JSON must be object"
          Nothing -> error "failed to parse context JSON"
      _ -> error "Usage: liquor-jekyll [JSON] [soure file]"
  Text.putStr text
  where
    go :: Context -> FilePath -> IO Text
    go context filePath = loadAndParseAndInterpret context filePath loadAndParse

    loadAndParse :: FilePath -> IO (Result JekyllTemplate, Context)
    loadAndParse filePath' = do
      (t, c) <- load filePath'
      pure (parse t, c)
