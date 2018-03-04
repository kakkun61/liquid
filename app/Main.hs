{-# LANGUAGE OverloadedStrings #-}

import Text.Liquoh

import qualified Codec.Binary.UTF8.String as UTF8Codec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.IO as Text
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> go (Aeson.object []) filepath
    ["", filepath] -> go (Aeson.object []) filepath
    [ctxStr, filepath] ->
      case Aeson.decode (ByteString.pack $ UTF8Codec.encode ctxStr) of
        Just context -> go context filepath
        Nothing -> hPutStrLn stderr "failed to parse context JSON" >> exitFailure
    _ -> hPutStrLn stderr "Usage liquoh [JSON] [soure file]" >> exitFailure
  where
    go context filepath = do
      source <-
        case filepath of
          "--" -> Text.getContents
          _ -> Text.readFile filepath
      case parseAndInterpret context source of
        Right t -> Text.putStr t
        Left err -> Text.putStrLn err >> exitFailure
