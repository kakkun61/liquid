{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Liquoh.Interpreter.Template where

import Control.Lens hiding (Context)
import qualified Data.Aeson as Aeson
import qualified Data.Convertible as Convertible
import Data.Foldable (foldl')
import Data.Monoid
import qualified Data.Text as Text

import Text.Liquoh.Interpreter
import Text.Liquoh.Interpreter.Expression hiding (Inject, inject, Shopify, at)
import qualified Text.Liquoh.Interpreter.Expression as E

newtype Template f g = Inject (g (Template f g))

foldTemplate :: Functor f => (f a -> a) -> Template e f -> a
foldTemplate f (Inject t) = f (fmap (foldTemplate f) t)

class Functor f => Interpret f where
  interpretAlgebra :: f (Context -> Result (Context, Text.Text)) -> Context -> Result (Context, Text.Text)

instance (Interpret f, Interpret g) => Interpret (f :+: g) where
  interpretAlgebra (InjectLeft x) = interpretAlgebra x
  interpretAlgebra (InjectRight x) = interpretAlgebra x

interpret :: (Evaluate f, Interpret g) => Context -> [Template (Expression f) g] -> Result (Context, Text.Text)
interpret c ts =
  go c ts Text.empty
  where
    go :: (Evaluate f, Interpret g) => Context -> [Template (Expression f) g] -> Text.Text -> Result (Context, Text.Text)
    go c' (t:r) acc =
      case foldTemplate interpretAlgebra t c' of
        Right (c'', a) -> go c'' r (acc <> a)
        f -> f
    go c' [] acc = Right (c', acc)

inject :: g :<: f => g (Template (Expression h) f) -> Template (Expression h) f
inject = Inject . inj

type Shopify = Plain :+: Output E.Shopify :+: If E.Shopify :+: Case E.Shopify :+: For E.Shopify :+: Assign E.Shopify

-- Plain Text

data Plain t = Plain Text.Text
  deriving Functor

instance Interpret Plain where
  interpretAlgebra (Plain x) c = Right $ (c, x)

plain :: Plain :<: f => Text.Text -> Template (Expression g) f
plain = inject . Plain

-- Output

data Output e t = Output (Expression e)
  deriving Functor

instance Evaluate e => Interpret (Output e) where
  interpretAlgebra (Output e) c = do
    v <- evaluate c e
    pure (c, render v)

output :: Output e :<: f => Expression e -> Template (Expression e) f
output = inject . Output

-- If

data If e t = If [(Expression e, t)]
  deriving Functor

instance Evaluate e => Interpret (If e) where
  interpretAlgebra (If ((p, t):r)) c = do
    v <- evaluate c p
    if asBool v
      then t c
      else interpretAlgebra (If r) c
  interpretAlgebra (If []) c = Right $ (c, Text.empty)

if_ :: If e :<: f => [(Expression e, Template (Expression e) f)] -> Template (Expression e) f
if_ = inject . If

-- Case

data Case e t = Case (Expression e) [(Expression e, t)]
  deriving Functor

instance Evaluate e => Interpret (Case e) where
  interpretAlgebra (Case e cs) c = do
    v <- evaluate c e
    go v cs c
    where
      go :: Evaluate e => ValueData -> [(Expression e, Context -> Result (Context, Text.Text))] ->  Context -> Result (Context, Text.Text)
      go _ [] c' = Right (c', Text.empty)
      go v ((p, t):r) c' = do
        pv <- evaluate c' p
        if v == pv
          then t c'
          else go v r c'

case_ :: Case e :<: f => Expression e -> [(Expression e, Template (Expression e) f)] -> Template (Expression e) f
case_ e cs = inject $ Case e cs

-- For

data For e t = For Text.Text (Expression e) [t]
  deriving Functor

instance Evaluate e => Interpret (For e) where
  interpretAlgebra (For n e ts) c = do
    v <- evaluate c e
    case v of
      Array a ->
        foldl' eachArrayElement (Right (c, Text.empty)) a
      _ -> Left "only array can be iterated"
    where
      eachArrayElement :: Result (Context, Text.Text) -> ValueData -> Result (Context, Text.Text)
      eachArrayElement (Right (c', r)) v = do
        c'' <- attachContext c' n v
        foldl' eachSubTemplate (Right (c'', r)) ts
      eachArrayElement (Left err) _ = Left err

      eachSubTemplate :: Result (Context, Text.Text) -> (Context -> Result (Context, Text.Text)) -> Result (Context, Text.Text)
      eachSubTemplate (Right (c', r)) f = do
        (c'', r') <- f c'
        pure (c'', r <> r')
      eachSubTemplate (Left err) _ = Left err

for :: For e :<: f => Text.Text -> Expression e -> [Template (Expression e) f] -> Template (Expression e) f
for n e ts = inject $ For n e ts

-- Assign

data Assign e t = Assign Text.Text (Expression e)
  deriving Functor

instance Evaluate e => Interpret (Assign e) where
  interpretAlgebra (Assign n e) c = do
    v <- evaluate c e
    c' <- attachContext c n v
    pure (c', Text.empty)

assign :: Assign e :<: f => Text.Text -> Expression e -> Template (Expression e) f
assign n e = inject $ Assign n e

-- Others

attachContext :: Context -> Text.Text -> ValueData -> Result Context
attachContext (Aeson.Object o) n v = Right $ Aeson.Object $ o & at n ?~ Convertible.convert v
attachContext _ _ _ = Left "the root of context must be map"

unsafeAttachContext :: Context -> Text.Text -> ValueData -> Context
unsafeAttachContext c n v =
  case attachContext c n v of
    Right c' -> c'
    Left err -> error $ Text.unpack err
