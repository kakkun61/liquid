{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Binary.Liquor.Statement () where

import Prelude hiding (String)
import Data.Binary (Binary, get, put)
import Data.Vector.Binary ()

import Text.Liquor.Interpreter.Expression (Expression)
import Text.Liquor.Interpreter.Statement

instance Binary (s (StatementInject e s)) => Binary (StatementInject e s) where
  put (Inject v) = put v
  get = Inject <$> get

instance Binary (Plain t) where
  put (Plain v) = put v
  get = Plain <$> get

instance Binary (Expression e) => Binary (Output e t) where
  put (Output v) = put v
  get = Output <$> get

instance (Binary (Expression e), Binary t) => Binary (If e t) where
  put (If v) = put v
  get = If <$> get

instance (Binary (Expression e), Binary t) => Binary (Case e t) where
  put (Case v u) = put v >> put u
  get = Case <$> get <*> get

instance (Binary (Expression e), Binary t) => Binary (For e t) where
  put (For v u t) = put v >> put u >> put t
  get = For <$> get <*> get <*> get

instance Binary (Expression e) => Binary (Assign e t) where
  put (Assign v u) = put v >> put u
  get = Assign <$> get <*> get
