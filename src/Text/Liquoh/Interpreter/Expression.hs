{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Text.Liquoh.Interpreter.Expression where

import Prelude hiding (Bool, String)
import qualified Prelude
import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Lens
import qualified Data.Convertible as Convertible
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Text.Liquoh.Interpreter

newtype Expression f = Inject (f (Expression f))

foldExpression :: Functor f => (f a -> a) -> Expression f -> a
foldExpression f (Inject t) = f (fmap (foldExpression f) t)

class Functor f => Evaluate f where
  evaluateAlgebra :: Context -> f (Result ValueData) -> Result ValueData

instance (Evaluate f, Evaluate g) => Evaluate (f :+: g) where
  evaluateAlgebra c (InjectLeft x) = evaluateAlgebra c x
  evaluateAlgebra c (InjectRight x) = evaluateAlgebra c x

evaluate :: Evaluate f => Context -> Expression f -> Result ValueData
evaluate c = foldExpression (evaluateAlgebra c)

inject :: g :<: f => g (Expression f) -> Expression f
inject = Inject . inj

type Shopify = Value :+: Variable :+: Less :+: LessEqual :+: Grater :+: GraterEqual :+: Equal :+: NotEqual :+: And :+: Or :+: ArrayAt

-- Value

data ValueData
  = Number !Scientific.Scientific
  | String !Text.Text
  | Bool !Prelude.Bool
  | Nil
  | Array !(Vector.Vector ValueData)
  deriving (Show, Eq)

newtype Value e = Value ValueData
  deriving Functor

instance Evaluate Value where
  evaluateAlgebra _ (Value v) = Right v

number :: Value :<: f => Scientific.Scientific -> Expression f
number = inject . Value . Number

string :: Value :<: f => Text.Text -> Expression f
string = inject . Value . String

bool :: Value :<: f => Prelude.Bool -> Expression f
bool = inject . Value . Bool

nil :: Value :<: f => Expression f
nil = inject $ Value Nil

array :: Value :<: f => Vector.Vector ValueData -> Expression f
array = inject . Value . Array

--- Bool

asBool :: ValueData -> Prelude.Bool
asBool (Bool v) = v
asBool Nil = False
asBool _ = True

--- Aeson.Value

instance Convertible.Convertible Aeson.Value ValueData where
  safeConvert v@(Aeson.Object _) = Convertible.convError "variable must be assigned with value" v
  safeConvert (Aeson.Array a) = Array <$> traverse Convertible.safeConvert a
  safeConvert (Aeson.String t) = Right $ String t
  safeConvert (Aeson.Number n) = Right $ Number n
  safeConvert (Aeson.Bool b) = Right $ Bool b
  safeConvert Aeson.Null = Right Nil

instance Convertible.Convertible ValueData Aeson.Value where
  safeConvert (Array a) = Aeson.Array <$> traverse Convertible.safeConvert a
  safeConvert (String t) = Right $ Aeson.String t
  safeConvert (Number n) = Right $ Aeson.Number n
  safeConvert (Bool b) = Right $ Aeson.Bool b
  safeConvert Nil = Right Aeson.Null

--- Render

render :: ValueData -> Text.Text
render (Number n) =
  case Scientific.floatingOrInteger n of
    Left f -> Text.pack $ show (f :: Double)
    Right i -> Text.pack $ show (i :: Integer)
render (String t) = t
render (Bool True) = "true"
render (Bool False) = "false"
render Nil = ""
render (Array a) = Text.pack $ show a

-- Variable

data VariableData = VariableData VariablePath

newtype Variable e = Variable VariableData
  deriving Functor

instance Evaluate Variable where
  evaluateAlgebra c (Variable (VariableData p)) = evaluateVariable c p

type VariablePath = NonEmptyList.NonEmpty VariableName

data VariableName = ObjectKey Text.Text | ArrayKey Int
  deriving (Show, Eq)

variable :: Variable :<: f => VariablePath -> Expression f
variable = inject . Variable . VariableData

evaluateVariable :: Context -> VariablePath -> Result ValueData
evaluateVariable c p
  = either Left (either (Left . Text.pack . Convertible.convErrorMessage) Right . Convertible.safeConvert)
  $ maybe (Left "variable not found") Right
  $ c ^? build p
  where
    build :: Applicative f => VariablePath -> ((Context -> f Context) -> Context -> f Context)
    build xs = foldl1 (.) (matchKey <$> xs)
      where matchKey (ObjectKey i) = Lens.key i
            matchKey (ArrayKey i)  = Lens.nth i

-- Binary Operator

--- Less

data Less e = Less e e
  deriving Functor

instance Evaluate Less where
  evaluateAlgebra _ (Less (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x < y
  evaluateAlgebra _ (Less (Right (Number x)) (Right (Number y))) = Right $ Bool $ x < y
  evaluateAlgebra _ (Less (Right (String x)) (Right (String y))) = Right $ Bool $ x < y
  evaluateAlgebra _ (Less r@(Left _) _) = r
  evaluateAlgebra _ (Less _ r@(Left _)) = r
  evaluateAlgebra _ (Less _ _) = Left "invalid parameter for less"

(.<.) :: Less :<: f => Expression f -> Expression f -> Expression f
x .<. y = inject (Less x y)
infix 4 .<.

--- Less or Equal

data LessEqual e = LessEqual e e
  deriving Functor

instance Evaluate LessEqual where
  evaluateAlgebra _ (LessEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x <= y
  evaluateAlgebra _ (LessEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x <= y
  evaluateAlgebra _ (LessEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x <= y
  evaluateAlgebra _ (LessEqual r@(Left _) _) = r
  evaluateAlgebra _ (LessEqual _ r@(Left _)) = r
  evaluateAlgebra _ (LessEqual _ _) = Left "invalid parameter for less equal"

(.<=.) :: LessEqual :<: f => Expression f -> Expression f -> Expression f
x .<=. y = inject (LessEqual x y)
infix 4 .<=.

--- Grater

data Grater e = Grater e e
  deriving Functor

instance Evaluate Grater where
  evaluateAlgebra _ (Grater (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x > y
  evaluateAlgebra _ (Grater (Right (Number x)) (Right (Number y))) = Right $ Bool $ x > y
  evaluateAlgebra _ (Grater (Right (String x)) (Right (String y))) = Right $ Bool $ x > y
  evaluateAlgebra _ (Grater r@(Left _) _) = r
  evaluateAlgebra _ (Grater _ r@(Left _)) = r
  evaluateAlgebra _ (Grater _ _) = Left "invalid parameter for grater"

(.>.) :: Grater :<: f => Expression f -> Expression f -> Expression f
x .>. y = inject (Grater x y)
infix 4 .>.

--- Grater or Equal

data GraterEqual e = GraterEqual e e
  deriving Functor

instance Evaluate GraterEqual where
  evaluateAlgebra _ (GraterEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x >= y
  evaluateAlgebra _ (GraterEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x >= y
  evaluateAlgebra _ (GraterEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x >= y
  evaluateAlgebra _ (GraterEqual r@(Left _) _) = r
  evaluateAlgebra _ (GraterEqual _ r@(Left _)) = r
  evaluateAlgebra _ (GraterEqual _ _) = Left "invalid parameter for grater equal"

(.>=.) :: GraterEqual :<: f => Expression f -> Expression f -> Expression f
x .>=. y = inject (GraterEqual x y)
infix 4 .>=.

--- Equal

data Equal e = Equal e e
  deriving Functor

instance Evaluate Equal where
  evaluateAlgebra _ (Equal (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x == y
  evaluateAlgebra _ (Equal (Right (Number x)) (Right (Number y))) = Right $ Bool $ x == y
  evaluateAlgebra _ (Equal (Right (String x)) (Right (String y))) = Right $ Bool $ x == y
  evaluateAlgebra _ (Equal r@(Left _) _) = r
  evaluateAlgebra _ (Equal _ r@(Left _)) = r
  evaluateAlgebra _ (Equal _ _) = Left "invalid parameter for equal"

(.==.) :: Equal :<: f => Expression f -> Expression f -> Expression f
x .==. y = inject (Equal x y)
infix 4 .==.

--- Not Equal

data NotEqual e = NotEqual e e
  deriving Functor

instance Evaluate NotEqual where
  evaluateAlgebra _ (NotEqual (Right (Bool x)) (Right (Bool y))) = Right $ Bool $ x /= y
  evaluateAlgebra _ (NotEqual (Right (Number x)) (Right (Number y))) = Right $ Bool $ x /= y
  evaluateAlgebra _ (NotEqual (Right (String x)) (Right (String y))) = Right $ Bool $ x /= y
  evaluateAlgebra _ (NotEqual r@(Left _) _) = r
  evaluateAlgebra _ (NotEqual _ r@(Left _)) = r
  evaluateAlgebra _ (NotEqual _ _) = Left "invalid parameter for not equal"

(./=.) :: NotEqual :<: f => Expression f -> Expression f -> Expression f
x ./=. y = inject (NotEqual x y)
infix 4 ./=.

--- And

data And e = And e e
  deriving Functor

instance Evaluate And where
  evaluateAlgebra _ (And (Right x) (Right y)) = Right $ Bool $ asBool x && asBool y
  evaluateAlgebra _ (And r@(Left _) _) = r
  evaluateAlgebra _ (And _ r@(Left _)) = r

(.&&.) :: And :<: f => Expression f -> Expression f -> Expression f
x .&&. y = inject (And x y)
infix 3 .&&.

--- Or

data Or e = Or e e
  deriving Functor

instance Evaluate Or where
  evaluateAlgebra _ (Or (Right x) (Right y)) = Right $ Bool $ asBool x && asBool y
  evaluateAlgebra _ (Or r@(Left _) _) = r
  evaluateAlgebra _ (Or _ r@(Left _)) = r

(.||.) :: Or :<: f => Expression f -> Expression f -> Expression f
x .||. y = inject (Or x y)
infix 2 .||.

-- Array

--- At

data ArrayAt e = ArrayAt e e
  deriving Functor

instance Evaluate ArrayAt where
  evaluateAlgebra _ (ArrayAt (Right (Array a)) (Right (Number i)))
    | Scientific.isInteger i = maybe (Left "index out of range") Right $ a Vector.!? round i
    | otherwise = Left "index must be integer"
  evaluateAlgebra _ (ArrayAt r@(Left _) _) = r
  evaluateAlgebra _ (ArrayAt _ r@(Left _)) = r
  evaluateAlgebra _ _ = Left "invalid parameter for array at"

at :: ArrayAt :<: f => Expression f -> Expression f -> Expression f
at x y = inject (ArrayAt x y)
infix 5 `at`
