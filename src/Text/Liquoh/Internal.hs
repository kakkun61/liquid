{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
= BNF

@
template := plain-text
            {{ expression }}
            {{ expression | filter
            {% tag %}

value := \/-?[1-9][0-9]*(.[0-9]*)?\/ # number
         \/"[A-z_]+[A-z_0-9]*"\/ # string
         true
         false
         nil
         array

variable := variable-name
            variable.variable-name

variable-name := \/[A-z]+\/

tag := # comment
       comment
       endcomment

       # control flow
       if expression
       endif
       unless exression
       endunless
       elsif expression
       else
       case expression
       when expression
       endcase

       # iteration
       for variable-name in expresson
       endfor
       break
       continue
       cycle expression-list
       cycle expression: expression-list
       tablerow variable-name in expression

       # raw
       raw
       endraw

       # variable
       assign variable-name = expression
       capture variable-name
       endcapture
       increment variable-name
       decrement variable-name

expression := value
              variable

              # binary operator
              expression < expression
              expression <= expression
              expression > expression
              expression >= expression
              expression == expression
              expression != expression
              expression && expression
              expression || expression
              expression contains expression

              # array
              expression[expression]
              expression array-parameters
              (expression..expression)

array := expression-without-array-without-parenthesis, array

expression-without-raw-array := expression \ array

array-parameters := limit:exression array-parameters
                    offset:expression array-parameters
                    reversed array-parameters

filter := abs
          append expression
@
-}
module Text.Liquoh.Internal where

import Prelude hiding (Bool, String)
import qualified Prelude
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype Expression f = Inject (f (Expression f))

data (f :+: g) e = InjectLeft (f e) | InjectRight (g e)
  deriving (Show, Functor)
infixr 8 :+:

foldExpr :: Functor f => (f a -> a) -> Expression f -> a
foldExpr f (Inject t) = f (fmap (foldExpr f) t)

type Result = Either Text.Text

class Functor f => Evaluate f where
  evalAlgebra :: f (Result ValueData) -> Result ValueData

instance (Evaluate f, Evaluate g) => Evaluate (f :+: g) where
  evalAlgebra (InjectLeft x) = evalAlgebra x
  evalAlgebra (InjectRight x) = evalAlgebra x

evaluate :: Evaluate f => Expression f -> Result ValueData
evaluate = foldExpr evalAlgebra

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f ~ g) => f :<: g where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InjectLeft

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InjectRight . inj

inject :: (g :<: f) => g (Expression f) -> Expression f
inject = Inject . inj

type Liquid = Value :+: Less :+: LessEqual :+: Grater :+: GraterEqual :+: Equal :+: NotEqual :+: And :+: Or :+: ArrayAt

-- Value

data ValueData
  = Number Double
  | String Text.Text
  | Bool Prelude.Bool
  | Nil
  | Array (Vector.Vector ValueData)
  deriving (Show, Eq)

newtype Value e = Value ValueData
  deriving (Functor)

instance Evaluate Value where
  evalAlgebra (Value v) = Right v

number :: (Value :<: f) => Double -> Expression f
number = inject . Value . Number

string :: (Value :<: f) => Text.Text -> Expression f
string = inject . Value . String

bool :: (Value :<: f) => Prelude.Bool -> Expression f
bool = inject . Value . Bool

nil :: (Value :<: f) => Expression f
nil = inject $ Value Nil

array :: (Value :<: f) => Vector.Vector ValueData -> Expression f
array = inject . Value . Array

-- Binary Operator

--- Less

data Less e = Less e e
  deriving (Functor)

instance Evaluate Less where
  evalAlgebra (Less (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x < y
  evalAlgebra (Less (Right (Number x)) (Right (Number y))) = Right $ Bool $ x < y
  evalAlgebra (Less (Right (String x)) (Right (String y))) = Right $ Bool $ x < y
  evalAlgebra (Less r@(Left _) _) = r
  evalAlgebra (Less _ r@(Left _)) = r
  evalAlgebra (Less _ _) = Left "invalid parameter for less"

(.<.) :: (Less :<: f) => Expression f -> Expression f -> Expression f
x .<. y = inject (Less x y)
infix 4 .<.

--- Less or Equal

data LessEqual e = LessEqual e e
  deriving (Functor)

instance Evaluate LessEqual where
  evalAlgebra (LessEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x <= y
  evalAlgebra (LessEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x <= y
  evalAlgebra (LessEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x <= y
  evalAlgebra (LessEqual r@(Left _) _) = r
  evalAlgebra (LessEqual _ r@(Left _)) = r
  evalAlgebra (LessEqual _ _) = Left "invalid parameter for less equal"

(.<=.) :: (LessEqual :<: f) => Expression f -> Expression f -> Expression f
x .<=. y = inject (LessEqual x y)
infix 4 .<=.

--- Grater

data Grater e = Grater e e
  deriving (Functor)

instance Evaluate Grater where
  evalAlgebra (Grater (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x > y
  evalAlgebra (Grater (Right (Number x)) (Right (Number y))) = Right $ Bool $ x > y
  evalAlgebra (Grater (Right (String x)) (Right (String y))) = Right $ Bool $ x > y
  evalAlgebra (Grater r@(Left _) _) = r
  evalAlgebra (Grater _ r@(Left _)) = r
  evalAlgebra (Grater _ _) = Left "invalid parameter for grater"

(.>.) :: (Grater :<: f) => Expression f -> Expression f -> Expression f
x .>. y = inject (Grater x y)
infix 4 .>.

--- Grater or Equal

data GraterEqual e = GraterEqual e e
  deriving (Functor)

instance Evaluate GraterEqual where
  evalAlgebra (GraterEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x >= y
  evalAlgebra (GraterEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x >= y
  evalAlgebra (GraterEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x >= y
  evalAlgebra (GraterEqual r@(Left _) _) = r
  evalAlgebra (GraterEqual _ r@(Left _)) = r
  evalAlgebra (GraterEqual _ _) = Left "invalid parameter for grater equal"

(.>=.) :: (GraterEqual :<: f) => Expression f -> Expression f -> Expression f
x .>=. y = inject (GraterEqual x y)
infix 4 .>=.

--- Equal

data Equal e = Equal e e
  deriving (Functor)

instance Evaluate Equal where
  evalAlgebra (Equal (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x == y
  evalAlgebra (Equal (Right (Number x)) (Right (Number y))) = Right $ Bool $ x == y
  evalAlgebra (Equal (Right (String x)) (Right (String y))) = Right $ Bool $ x == y
  evalAlgebra (Equal r@(Left _) _) = r
  evalAlgebra (Equal _ r@(Left _)) = r
  evalAlgebra (Equal _ _) = Left "invalid parameter for equal"

(.==.) :: (Equal :<: f) => Expression f -> Expression f -> Expression f
x .==. y = inject (Equal x y)
infix 4 .==.

--- Not Equal

data NotEqual e = NotEqual e e
  deriving (Functor)

instance Evaluate NotEqual where
  evalAlgebra (NotEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x /= y
  evalAlgebra (NotEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x /= y
  evalAlgebra (NotEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x /= y
  evalAlgebra (NotEqual r@(Left _) _) = r
  evalAlgebra (NotEqual _ r@(Left _)) = r
  evalAlgebra (NotEqual _ _) = Left "invalid parameter for not equal"

(./=.) :: (NotEqual :<: f) => Expression f -> Expression f -> Expression f
x ./=. y = inject (NotEqual x y)
infix 4 ./=.

--- And

data And e = And e e
  deriving (Functor)

instance Evaluate And where
  evalAlgebra (And (Right x) (Right y)) =
    let
      x' = asBool x
      y' = asBool y
    in
      Right $ Bool $ x' && y'
  evalAlgebra (And r@(Left _) _) = r
  evalAlgebra (And _ r@(Left _)) = r

(.&&.) :: (And :<: f) => Expression f -> Expression f -> Expression f
x .&&. y = inject (And x y)
infix 3 .&&.

--- Or

data Or e = Or e e
  deriving (Functor)

instance Evaluate Or where
  evalAlgebra (Or (Right x) (Right y)) =
    let
      x' = asBool x
      y' = asBool y
    in
      Right $ Bool $ x' || y'
  evalAlgebra (Or r@(Left _) _) = r
  evalAlgebra (Or _ r@(Left _)) = r

(.||.) :: (Or :<: f) => Expression f -> Expression f -> Expression f
x .||. y = inject (Or x y)
infix 2 .||.

-- Array

--- At

data ArrayAt e = ArrayAt e e
  deriving (Functor)

instance Evaluate ArrayAt where
  evalAlgebra (ArrayAt (Right (Array a)) (Right (Number i)))
    | isInt i = maybe (Left "index out of range") Right $ a Vector.!? round i
    | otherwise = Left "index must be integer"
  evalAlgebra (ArrayAt r@(Left _) _) = r
  evalAlgebra (ArrayAt _ r@(Left _)) = r
  evalAlgebra _ = Left "invalid parameter for array at"

at :: (ArrayAt :<: f) => Expression f -> Expression f -> Expression f
at x y = inject (ArrayAt x y)
infix 5 `at`

-- Others

asBool :: ValueData -> Prelude.Bool
asBool (Bool v) = v
asBool Nil = False
asBool _ = True

isInt :: Double -> Prelude.Bool
isInt x = x == fromInteger (round x)
