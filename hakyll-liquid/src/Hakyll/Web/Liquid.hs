module Hakyll.Web.Liquid
  ( parseAndInterpretDefault
  , parseAndInterpret
  , parseAndInterpret'
  , parse
  , interpret
  , interpret'
  ) where

import Control.Monad.Error.Class
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Hakyll
import Hakyll.Core.Compiler
import qualified Text.Liquoh as Liquid

-- | Parse underlying item and compile it with its metadata as context.
parseAndInterpretDefault :: Compiler (Item String)
parseAndInterpretDefault = do
  metadata <- getMetadata =<< getUnderlying
  item <- getResourceBody
  parse item >>= interpret metadata

-- | Parse underlying item and compile it with given metadata as context.
parseAndInterpret :: Metadata -> Compiler (Item String)
parseAndInterpret metadata = getResourceBody >>= parse >>= interpret metadata

-- | Parse underlying item and compile it with given context.
parseAndInterpret' :: Aeson.Value -> Compiler (Item String)
parseAndInterpret' context = getResourceBody >>= parse >>= interpret' context

-- | Parse given item.
parse :: Item String -> Compiler (Item [Liquid.ShopifyTemplate])
parse (Item identifier body) =
  case Liquid.parse (Text.pack body) of
    Right template -> return $ Item identifier template
    Left err -> throwError [Text.unpack err]

-- | Compile Liquid expressions with given metadata as context.
interpret :: Metadata -> Item [Liquid.ShopifyTemplate] -> Compiler (Item String)
interpret metadata = interpret' (Aeson.Object metadata)

-- | Compile Liquid expressions with given context.
interpret' :: Aeson.Value -> Item [Liquid.ShopifyTemplate] -> Compiler (Item String)
interpret' context (Item identifier template) =
  case Liquid.interpret context template of
    Right text -> return $ Item identifier $ Text.unpack text
    Left err -> throwError [Text.unpack err]
