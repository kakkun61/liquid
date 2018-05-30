{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Liquor.Statement () where

import Prelude hiding (String)
import Data.Binary (Binary, Get, get, put)
import Data.Vector.Binary ()
import Data.Word (Word8)

import Text.Liquor.Interpreter.Statement

