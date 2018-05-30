{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-methods #-}

module Text.Liquor.ParserSpec where

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

import Text.Liquor.Helper ()
import Text.Liquor.Interpreter (ShopifyExpression, ShopifyTemplate)
import Text.Liquor.Interpreter.Common
import qualified Text.Liquor.Interpreter.Expression as E
import qualified Text.Liquor.Interpreter.Statement as S
import Text.Liquor.Parser (expression, template)

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

mapEAdt :: MapEAdt e => E.Expression e -> Expression
mapEAdt = E.foldExpression mapEAdtAlgebra

class Functor e => MapEAdt e where
  mapEAdtAlgebra :: e Expression -> Expression

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

instance MapEAdt E.Greater where
  mapEAdtAlgebra (E.Greater a b) = Grater a b

instance MapEAdt E.GreaterEqual where
  mapEAdtAlgebra (E.GreaterEqual a b) = GraterEqual a b

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

data Statement
  = Plain Text.Text
  | Output Expression
  | If [(Expression, [Statement])]
  | Case Expression [(Expression, [Statement])]
  | For Text.Text Expression [Statement]
  | Assign Text.Text Expression
  deriving (Show, Eq)

mapTAdt :: (MapSAdt s, MapEAdt e) => S.Statement e s -> Statement
mapTAdt = S.foldStatement mapTAdtAlgebra

class Functor s => MapSAdt s where
  mapTAdtAlgebra :: s Statement -> Statement

instance (MapSAdt f, MapSAdt g) => MapSAdt (f :+: g) where
  mapTAdtAlgebra (InjectLeft x) = mapTAdtAlgebra x
  mapTAdtAlgebra (InjectRight x) = mapTAdtAlgebra x

instance MapSAdt S.Plain where
  mapTAdtAlgebra (S.Plain t) = Plain t

instance MapEAdt e => MapSAdt (S.Output e) where
  mapTAdtAlgebra (S.Output e) = Output $ mapEAdt e

instance MapEAdt e => MapSAdt (S.If e) where
  mapTAdtAlgebra (S.If x) = If $ first mapEAdt <$> x

instance MapEAdt e => MapSAdt (S.Case e) where
  mapTAdtAlgebra (S.Case e x) = Case (mapEAdt e) (first mapEAdt <$> x)

instance MapEAdt e => MapSAdt (S.For e) where
  mapTAdtAlgebra (S.For t e ts) = For t (mapEAdt e) ts

instance MapEAdt e => MapSAdt (S.Assign e) where
  mapTAdtAlgebra (S.Assign t e) = Assign t (mapEAdt e)

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
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
        `shouldBe`
           Right output

    it "1\\n2\\n3" $ do
      let
        input = "1\n2\n3"
        output = [Plain "1\n2\n3"]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
        `shouldBe`
           Right output

    it "{{ 1 }}" $ do
      let
        input = "{{ 1 }}"
        output = [Output 1]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
        `shouldBe`
           Right output

    it "{% if 1 %}true{% endif %}" $ do
      let
        input = "{% if 1 %}true{% endif %}"
        output =
          [If [(1, [Plain "true"])]]
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
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
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
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
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
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
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
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
      (mapTAdt <$>) <$> (parseOnly template input :: Either Prelude.String ShopifyTemplate)
        `shouldBe`
           Right output
