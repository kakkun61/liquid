{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Text.Liquor.Interpreter.Statement where

import Control.Lens hiding (Context)
import qualified Data.Convertible as Convertible
import Data.Foldable (foldl')
import Data.Monoid
import qualified Data.Text as Text

import Text.Liquor.Common
import Text.Liquor.Interpreter.Common
import Text.Liquor.Interpreter.Expression hiding (Inject, inject, Shopify, at)
import qualified Text.Liquor.Interpreter.Expression as E

newtype StatementInject e s = Inject (s (StatementInject e s))

type Statement e s = StatementInject (Expression e) s

type Template e s = [Statement e s]

foldStatement :: Functor s => (s a -> a) -> Statement e s -> a
foldStatement s (Inject t) = s (fmap (foldStatement s) t)

class Functor s => Interpret s where
  interpretAlgebra :: s (Context -> Result (Context, Text.Text)) -> Context -> Result (Context, Text.Text)

instance (Interpret f, Interpret g) => Interpret (f :+: g) where
  interpretAlgebra (InjectLeft x) = interpretAlgebra x
  interpretAlgebra (InjectRight x) = interpretAlgebra x

interpret :: (Evaluate e, Interpret s) => Context -> Template e s -> Result Text.Text
interpret c ts = snd <$> interpret' c ts

interpret' :: (Evaluate e, Interpret s) => Context -> [Statement e s] -> Result (Context, Text.Text)
interpret' c ts =
  foldl' go (Right (c, Text.empty)) ts
  where
    go :: (Evaluate e, Interpret s) => Result (Context, Text.Text) -> Statement e s -> Result (Context, Text.Text)
    go (Right (c', acc)) t = do
      (c'', a) <- foldStatement interpretAlgebra t c'
      pure (c'', acc <> a)
    go r@(Left _) _ = r

inject :: g :<: f => g (Statement e f) -> Statement e f
inject = Inject . inj

type Shopify = Plain :+: Output E.Shopify :+: If E.Shopify :+: Case E.Shopify :+: For E.Shopify :+: Assign E.Shopify
type ShopifySuper e t =
  (Plain :<: t, Output e :<: t, If e :<: t, Case e :<: t, For e :<: t, Assign e :<: t)

-- Plain Text

data Plain t = Plain Text.Text
  deriving Functor

instance Interpret Plain where
  interpretAlgebra (Plain x) c = Right $ (c, x)

plain :: Plain :<: s => Text.Text -> Statement e s
plain = inject . Plain

-- Output

data Output e t = Output (Expression e)
  deriving Functor

instance Evaluate e => Interpret (Output e) where
  interpretAlgebra (Output e) c = do
    v <- evaluate c e
    pure (c, render v)

output :: Output e :<: s => Expression e -> Statement e s
output = inject . Output

-- If

data If e t = If [(Expression e, [t])]
  deriving Functor

instance Evaluate e => Interpret (If e) where
  interpretAlgebra (If ((p, ts):r)) c = do
    v <- evaluate c p
    if asBool v
      then interpretStatements ts c
      else interpretAlgebra (If r) c
  interpretAlgebra (If []) c = Right $ (c, Text.empty)

if_ :: If e :<: s => [(Expression e, [Statement e s])] -> Statement e s
if_ = inject . If

-- Case

data Case e t = Case (Expression e) [(Expression e, [t])]
  deriving Functor

instance Evaluate e => Interpret (Case e) where
  interpretAlgebra (Case e cs) c = do
    v <- evaluate c e
    go v cs c
    where
      go :: Evaluate e => ValueData -> [(Expression e, [Context -> Result (Context, Text.Text)])] ->  Context -> Result (Context, Text.Text)
      go _ [] c' = Right (c', Text.empty)
      go v ((p, ts):r) c' = do
        pv <- evaluate c' p
        if v == pv
          then interpretStatements ts c'
          else go v r c'

case_ :: Case e :<: s => Expression e -> [(Expression e, [Statement e s])] -> Statement e s
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
        foldl' eachSubStatement (Right (c'', r)) ts
      eachArrayElement (Left err) _ = Left err

      eachSubStatement :: Result (Context, Text.Text) -> (Context -> Result (Context, Text.Text)) -> Result (Context, Text.Text)
      eachSubStatement (Right (c', r)) f = do
        (c'', r') <- f c'
        pure (c'', r <> r')
      eachSubStatement (Left err) _ = Left err

for :: For e :<: s => Text.Text -> Expression e -> [Statement e s] -> Statement e s
for n e ts = inject $ For n e ts

-- Assign

data Assign e t = Assign Text.Text (Expression e)
  deriving Functor

instance Evaluate e => Interpret (Assign e) where
  interpretAlgebra (Assign n e) c = do
    v <- evaluate c e
    c' <- attachContext c n v
    pure (c', Text.empty)

assign :: Assign e :<: s => Text.Text -> Expression e -> Statement e s
assign n e = inject $ Assign n e

-- Others

attachContext :: Context -> Text.Text -> ValueData -> Result Context
attachContext o n v = Right $ o & at n ?~ Convertible.convert v

unsafeAttachContext :: Context -> Text.Text -> ValueData -> Context
unsafeAttachContext c n v =
  case attachContext c n v of
    Right c' -> c'
    Left err -> error $ Text.unpack err

interpretStatements :: [Context -> Result (Context, Text.Text)] -> Context -> Result (Context, Text.Text)
interpretStatements ts c =
  foldl' eachSubStatement (Right (c, Text.empty)) ts
  where
    eachSubStatement :: Result (Context, Text.Text) -> (Context -> Result (Context, Text.Text)) -> Result (Context, Text.Text)
    eachSubStatement (Right (c', r)) f = do
      (c'', r') <- f c'
      pure (c'', r <> r')
    eachSubStatement (Left err) _ = Left err
