module Text.Liquor.Common where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

type Result = Either Text.Text

type Context = Aeson.Object
