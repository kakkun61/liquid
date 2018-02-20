{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-methods #-}

module Text.Liquoh.ParserSpec where

import Prelude hiding (Bool, String)
import qualified Prelude
import Test.Hspec
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text (parseOnly)
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Scientific as Scientific
import Data.String
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Text.Liquoh.Helper ()
import Text.Liquoh.Interpreter (ShopifyExpression, ShopifyTemplate)
import Text.Liquoh.Interpreter.Common
import qualified Text.Liquoh.Interpreter.Expression as E
import qualified Text.Liquoh.Interpreter.Template as T
import Text.Liquoh.Parser (expression, template)

-- AST ADT

data Expression
  = Number !Scientific.Scientific
  | String !Text.Text
  | Bool !Prelude.Bool
  | Nil
  | Array !(Vector.Vector E.ValueData)
  | Variable E.VariablePath
  | Less Expression Expression
  | LessEqual Expression Expression
  | Grater Expression Expression
  | GraterEqual Expression Expression
  | Equal Expression Expression
  | NotEqual Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | ArrayAt Expression Expression
  deriving (Show, Eq)

instance IsString Expression where
  fromString = String . fromString

instance Num Expression where
  fromInteger = Number . fromInteger

(.<.), (.<=.), (.>.), (.>=.), (.==.), (./=.), (.&&.), (.||.) :: Expression -> Expression -> Expression
infix 4 .<., .<=., .>., .>=., .==., ./=.
infix 3 .&&.
infix 2 .||.
(.<.) = Less
(.<=.) = LessEqual
(.>.) = Grater
(.>=.) = GraterEqual
(.==.) = Equal
(./=.) = NotEqual
(.&&.) = And
(.||.) = Or

mapEAdt :: MapEAdt f => E.Expression f -> Expression
mapEAdt = E.foldExpression mapEAdtAlgebra

class Functor f => MapEAdt f where
  mapEAdtAlgebra :: f Expression -> Expression

instance (MapEAdt f, MapEAdt g) => MapEAdt (f :+: g) where
  mapEAdtAlgebra (InjectLeft x) = mapEAdtAlgebra x
  mapEAdtAlgebra (InjectRight x) = mapEAdtAlgebra x

instance MapEAdt E.Value where
  mapEAdtAlgebra (E.Value (E.Number n)) = Number n
  mapEAdtAlgebra (E.Value (E.String s)) = String s
  mapEAdtAlgebra (E.Value (E.Bool b)) = Bool b
  mapEAdtAlgebra (E.Value E.Nil) = Nil
  mapEAdtAlgebra (E.Value (E.Array a)) = Array a

instance MapEAdt E.Variable where
  mapEAdtAlgebra (E.Variable p) = Variable p

instance MapEAdt E.Less where
  mapEAdtAlgebra (E.Less a b) = Less a b

instance MapEAdt E.LessEqual where
  mapEAdtAlgebra (E.LessEqual a b) = LessEqual a b

instance MapEAdt E.Grater where
  mapEAdtAlgebra (E.Grater a b) = Grater a b

instance MapEAdt E.GraterEqual where
  mapEAdtAlgebra (E.GraterEqual a b) = GraterEqual a b

instance MapEAdt E.Equal where
  mapEAdtAlgebra (E.Equal a b) = Equal a b

instance MapEAdt E.NotEqual where
  mapEAdtAlgebra (E.NotEqual a b) = NotEqual a b

instance MapEAdt E.And where
  mapEAdtAlgebra (E.And a b) = And a b

instance MapEAdt E.Or where
  mapEAdtAlgebra (E.Or a b) = Or a b

instance MapEAdt E.ArrayAt where
  mapEAdtAlgebra (E.ArrayAt a b) = ArrayAt a b

data Template
  = Plain Text.Text
  | Output Expression
  | If [(Expression, [Template])]
  | Case Expression [(Expression, [Template])]
  | For Text.Text Expression [Template]
  | Assign Text.Text Expression
  deriving (Show, Eq)

mapTAdt :: (MapTAdt f, MapEAdt e) => T.Template (E.Expression e) f -> Template
mapTAdt = T.foldTemplate mapTAdtAlgebra

class Functor f => MapTAdt f where
  mapTAdtAlgebra :: f Template -> Template

instance (MapTAdt f, MapTAdt g) => MapTAdt (f :+: g) where
  mapTAdtAlgebra (InjectLeft x) = mapTAdtAlgebra x
  mapTAdtAlgebra (InjectRight x) = mapTAdtAlgebra x

instance MapTAdt T.Plain where
  mapTAdtAlgebra (T.Plain t) = Plain t

instance MapEAdt e => MapTAdt (T.Output e) where
  mapTAdtAlgebra (T.Output e) = Output $ mapEAdt e

instance MapEAdt e => MapTAdt (T.If e) where
  mapTAdtAlgebra (T.If x) = If $ first mapEAdt <$> x

instance MapEAdt e => MapTAdt (T.Case e) where
  mapTAdtAlgebra (T.Case e x) = Case (mapEAdt e) (first mapEAdt <$> x)

instance MapEAdt e => MapTAdt (T.For e) where
  mapTAdtAlgebra (T.For t e ts) = For t (mapEAdt e) ts

instance MapEAdt e => MapTAdt (T.Assign e) where
  mapTAdtAlgebra (T.Assign t e) = Assign t (mapEAdt e)

spec :: Spec
spec = do
  describe "expression" $ do
    it "1" $ do
      let
        input = "1"
        output = 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "\"hi\"" $ do
      let
        input = "\"hi\""
        output = "hi"
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "true" $ do
      let
        input = "true"
        output = Bool True
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "nil" $ do
      let
        input = "nil"
        output = Nil
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "x" $ do
      let
        context = Aeson.object ["x" .= Aeson.String "x"]
        input = "x"
        output = Variable (E.ObjectKey "x" :| [])
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 == 1" $ do
      let
        input = "1 == 1"
        output = 1 .==. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 != 1" $ do
      let
        input = "1 != 1"
        output = 1 ./=. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 < 1" $ do
      let
        input = "1 < 1"
        output = 1 .<. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 <= 1" $ do
      let
        input = "1 <= 1"
        output = 1 .<=. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 > 1" $ do
      let
        input = "1 > 1"
        output = 1 .>. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 >= 1" $ do
      let
        input = "1 >= 1"
        output = 1 .>=. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 and 1" $ do
      let
        input = "1 and 1"
        output = 1 .&&. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 or 1" $ do
      let
        input = "1 or 1"
        output = 1 .||. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 and 1 < 1" $ do
      let
        input = "1 and 1 < 1"
        output = 1 .&&. 1 .<. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

    it "1 == 1 and 1 < 2 or 1 < 1" $ do
      let
        input = "1 == 1 and 1 < 2 or 1 < 1"
        output = 1 .==. 1 .&&. 1 .<. 2 .||. 1 .<. 1
      mapEAdt <$> (parseOnly expression input :: Either Prelude.String ShopifyExpression)
        `shouldBe`
          Right output

  describe "template" $ do
    it "hello" $ do
      let
        input = "hello"
        output = [Plain "hello"]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "1\\n2\\n3" $ do
      let
        input = "1\n2\n3"
        output = [Plain "1\n2\n3"]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "{{ 1 }}" $ do
      let
        input = "{{ 1 }}"
        output = [Output 1]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "{% if 1 %}true{% endif %}" $ do
      let
        input = "{% if 1 %}true{% endif %}"
        output =
          [If [(1, [Plain "true"])]]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "{% if false %}true{% else %}false{% endif %}" $ do
      let
        input = "{% if false %}true{% else %}false{% endif %}"
        output =
          [ If
              [ (Bool False, [Plain "true"])
              , (Bool True, [Plain "false"])
              ]
          ]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "{% if false %}1{% elsif false %}2{% else %}3{% endif %}" $ do
      let
        input = "{% if false %}1{% elsif false %}2{% else %}3{% endif %}"
        output =
          [ If
              [ (Bool False, [Plain "1"])
              , (Bool False, [Plain "2"])
              , (Bool True, [Plain "3"])
              ]
          ]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "<html>\\n<body>\\n{% if true %}\\n  true\\n{% else %}\\n  false\\n{% endif %}\\n</body>\\n</html>\\n" $ do
      let
        input = "<html>\n<body>\n{% if true %}\n  true\n{% else %}\n  false\n{% endif %}\n</body>\n</html>\n"
        output =
          [ Plain "<html>\n<body>\n"
          , If
              [ (Bool True, [Plain "\n  true\n"])
              , (Bool True, [Plain "\n  false\n"])
              ]
          , Plain "\n</body>\n</html>\n"
          ]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output

    it "{% for foo in bar %}foo{% endfor %}" $ do
      let
        input = "{% for foo in bar %}{{ foo }}{% endfor %}"
        output =
          [ For
              "foo"
              (Variable $ E.ObjectKey "bar" :| [])
              [Output (Variable $ E.ObjectKey "foo" :| [])]
          ]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String [ShopifyTemplate])
        `shouldBe`
           Right output
