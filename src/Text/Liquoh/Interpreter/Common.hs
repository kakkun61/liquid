{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Liquoh.Interpreter.Common where

data (f :+: g) e = InjectLeft (f e) | InjectRight (g e)
  deriving (Show, Functor)
infixr 6 :+:

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
infix 4 :<:

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f ~ g) => f :<: g where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InjectLeft

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InjectRight . inj
