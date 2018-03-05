{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquoh.Parser where

import Prelude hiding (and, null, or, takeWhile)

import Control.Applicative
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Expr
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Attoparsec
import Data.List (intersperse)
import Data.List.NonEmpty (nonEmpty)
import Data.Scientific (toBoundedInteger)
import Data.Semigroup hiding (option)
import Data.Text (Text)
import qualified Data.Text as Text

import Text.Liquoh.Interpreter ((:<:), (.<.), (.<=.), (.>.), (.>=.), (.==.), (./=.), (.&&.), (.||.))
import qualified Text.Liquoh.Interpreter as I

-- Tokens

colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

dot :: Parser Char
dot = char '.'

dotDot :: Parser Text
dotDot = ".."

eq :: Parser Text
eq = "=="

assignSign :: Parser Char
assignSign = char '='

gt :: Parser Char
gt = char '>'

gtEq :: Parser Text
gtEq = ">="

lt :: Parser Char
lt = char '<'

ltEq :: Parser Text
ltEq = "<="

minus :: Parser Char
minus = char '-'

neq :: Parser Text
neq = "!="

openBracket, closedBracket :: Parser Char
openBracket = char '['
closedBracket = char ']'

openParenthesis, closedParenthesis :: Parser Char
openParenthesis = char '('
closedParenthesis = char ')'

outputStart, outputEnd :: Parser Text
outputStart = "{{"
outputEnd = "}}"

pipe :: Parser Char
pipe = char '|'

questionMark :: Parser Char
questionMark = char '?'

tagStart, tagEnd :: Parser Text
tagStart = "{%"
tagEnd = "%}"

underscore :: Parser Char
underscore = char '_'

and :: Parser Text
and = "and"

assign :: Parser Text
assign = "assign"

break, continue :: Parser Text
break = "break"
continue = "continue"

captureStart, captureEnd :: Parser Text
captureStart = "capture"
captureEnd = "endcapture"

caseStart, caseEnd :: Parser Text
caseStart = "case"
caseEnd = "endcase"

commentStart, commentEnd :: Parser Text
commentStart = "comment"
commentEnd = "endcomment"

contains :: Parser Text
contains = "contains"

cycle :: Parser Text
cycle = "cycle"

else_ :: Parser Text
else_ = "else"

elsif :: Parser Text
elsif = "elsif"

empty :: Parser Text
empty = "empty"
end :: Parser Text
end = "end"

forStart, forEnd :: Parser Text
forStart = "for"
forEnd = "endfor"

in_ :: Parser Text
in_ = "in"

ifKeyStart :: Parser Text
ifKeyStart = "ifkey"

ifStart, ifEnd :: Parser Text
ifStart = "if"
ifEnd = "endif"

nil :: Parser Text
nil = "nil"

or :: Parser Text
or = "or"

rawStart, rawEnd :: Parser Text
rawStart = "raw"
rawEnd = "endraw"

tableRowStart, tableRowEnd :: Parser Text
tableRowStart = "tablerow"
tableRowEnd = "endtablerow"

true, false :: Parser Text
true = "true"
false = "false"

unlessStart, unlessEnd :: Parser Text
unlessStart = "unless"
unlessEnd = "endunless"

when :: Parser Text
when = "when"

with :: Parser Text
with = "with"

-- Grammer

-- | Match middle parser, around explicit start and end parsers
between
  :: Parser b -- ^ open tag parser
  -> Parser b -- ^ close tag parser
  -> Parser a -- ^ match middle parser
  -> Parser a
between open close p = do
  _ <- open
  x <- p
  (close *> return x) <|> fail "Tag or output statement incomplete"

-- | Match parser between whitespace
stripped
  :: Parser a
  -> Parser a
stripped =
  between skipSpace skipSpace

-- | Match given parser for a tag
tag
  :: Parser a
  -> Parser a
tag p =
  between tagStart tagEnd (stripped p)

-- | Match given parser for output block
outputTag
  :: Parser a
  -> Parser a
outputTag p =
  between outputStart outputEnd (stripped p)

-- | Match given tag name (e.g. for, case) with following parser
tagWith
  :: Parser a -- ^ initial tag type, e.g. for
  -> Parser b -- ^ follow on parser, e.g. variable
  -> Parser b
tagWith tg p =
  tag $ tg *> skipSpace >> p

-- | Convert match into text
mapText
  :: Parser [Char]
  -> Parser Text
mapText =
  fmap Text.pack

-- | Match variables (without indices, including underscore or hash)
var :: Parser Text
var = mapText $ many1 $ letter <|> satisfy (inClass "_-")

-- | Parse a positive integer within square brackets, e.g. "[123]", NOT "[123.1]"
parseBoxedInt :: Parser Int
parseBoxedInt = do
    sc <- between openBracket closedBracket scientific
    case toBoundedInteger sc of
      Just i  -> if i >= 0 then return i else err
      Nothing -> err
  where err = fail "invalid variable (array) index, expecting a positive integer"

-- | Parse a variable section with an optional indexing
--   An array index MUST be preceded by an object index
--   ...hence Maybe do comprehension
varIndexSection :: Parser I.VariablePath
varIndexSection = do
  vs <- sepBy var dot
  i  <- many parseBoxedInt
  brokenChar <- openBracket <|> return '~'
  let ixs = do obs <- (nonEmpty (I.ObjectKey <$> vs))
               Just obs <> (nonEmpty (I.ArrayKey <$> i))
  if brokenChar == '[' then (fail "invalid array index - ill-typed") else case ixs of
    Just nel -> return nel
    Nothing  -> fail "invalid var index section"

-- | Parse a variable
variable :: I.Variable :<: e => Parser (I.Expression e)
variable = do
  sections <- sepBy1 varIndexSection dot
  return . I.variable $ foldl1 (<>) sections

-- | e.g. raw tag, comment tag
rawBodyTag :: Parser Text -- ^ start tag matcher
           -> Parser Text -- ^ end tag matcher
           -> Parser Text
rawBodyTag s e =
  s >> skipSpace *> (mapText $ manyTill anyChar (skipSpace >> e))

---- | Match interior of raw tag
--rawTag :: Raw :<: f => Parser (Expression f)
--rawTag = RawText <$> rawBodyTag (tag rawStart) (tag rawEnd)

---- | Match interior of comment tag
--commentTag :: Comment :<: f => Parser (Expression f)
--commentTag = rawBodyTag (tag commentStart) (tag commentEnd) *> pure Noop

-- | Match any raw text upto a tag/output start or the end of the input
plain :: I.Plain :<: t => Parser (I.Template (I.Expression e) t)
plain = I.plain <$> (mapText $ manyTill1 anyChar terminator)
  where terminator = lookAhead $ tagStart <|> outputStart <|> (endOfInput *> pure Text.empty)

-- | Force the first character to be valid, otherwise fail miserably
manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p e = (:) <$> p <*> s
    where s = (e *> pure []) <|> ((:) <$> p <*> s)

-- | Match a quoted string
quoteString :: I.Value :<: e => Parser (I.Expression e)
quoteString = do
  skipSpace
  beginTick <- satisfy (inClass "\'\"")
  qs        <- mapText $ manyTill anyChar (char beginTick)
  return $ I.string qs

expression
  :: I.ShopifyExpressionSuper e
  => Parser (I.Expression e)
expression =
  buildExpressionParser table term
  where
    table =
      [ {-[ Infix arrayOperator AssocLeft ]
      , -}[ Infix comparisonOperator AssocNone ]
      , [ Infix andOperator AssocLeft ]
      , [ Infix orOperator AssocLeft ]
      ]

comparisonOperator
  :: (I.Equal :<: e, I.NotEqual :<: e, I.GraterEqual :<: e, I.LessEqual :<: e, I.Grater :<: e, I.Less :<: e)
  => Parser (I.Expression e -> I.Expression e -> I.Expression e)
comparisonOperator =
  stripped $ eq *> pure (.==.) <|>
             neq *> pure (./=.) <|>
             gtEq *> pure (.>=.) <|>
             ltEq *> pure (.<=.) <|>
             (mapText . some $ gt) *> pure (.>.) <|>
             (mapText . some $ lt) *> pure (.<.) {- <|>
             contains *> pure I.contains -}

andOperator :: I.And :<: e => Parser (I.Expression e -> I.Expression e -> I.Expression e)
andOperator = stripped $ and *> pure (.&&.)

orOperator :: I.Or :<: e => Parser (I.Expression e -> I.Expression e -> I.Expression e)
orOperator = stripped $ or *> pure (.||.)

--arrayOperator ::
--arrayOperator = stripped $ comma *> pure ()

term :: (I.Value :<: e, I.Variable :<: e) => Parser (I.Expression e)
term = immediateValue <|> variable

-- | Match a immediate value
immediateValue :: I.Value :<: e => Parser (I.Expression e)
immediateValue =
  quoteString <|>
  (I.number <$> scientific) <|>
  I.nil <$ (stripped nil) <|>
  I.bool False <$ (stripped false) <|>
  I.bool True <$ (stripped true)

if_ :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser [I.Template (I.Expression e) t] -> Parser (I.Template (I.Expression e) t)
if_ nest = do
  c <- tagWith ifStart expression
  t <- nest
  e <- elseList nest <|> pure []
  _ <- tag ifEnd
  pure $ I.if_ $ (c, t):e

elseList :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser [I.Template (I.Expression e) t] -> Parser [(I.Expression e, [I.Template (I.Expression e) t])]
elseList nest =
  do
    _ <- tag else_
    t <- nest
    pure [(I.bool True, t)]
  <|> do
    c <- tagWith elsif expression
    t <- nest
    e <- elseList nest <|> pure []
    pure $ (c, t):e

for :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser [I.Template (I.Expression e) t] -> Parser (I.Template (I.Expression e) t)
for nest = do
  (v, e) <- tagWith forStart $ (,) <$> var <*> (stripped "in" *> expression)
  t <- nest
  _ <- tag forEnd
  pure $ I.for v e t

output :: (I.Output e :<: t, I.ShopifyExpressionSuper e) => Parser (I.Template (I.Expression e) t)
output = I.output <$> outputTag ({-filterBlock <|>-} expression)

templateElement :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser (I.Template (I.Expression e) t)
templateElement =
  choice
    [ output
    , if_ template
    , for template
    ] <?> "Block Parsing"

templateElementList :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser [I.Template (I.Expression e) t]
templateElementList = do
  e <- templateElement
  t <- template
  pure $ e:t

template :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Parser [I.Template (I.Expression e) t]
template =
  endOfInput *> pure []
  <|> (lookAhead (char '{') *> ((:) <$> templateElement <*> template <|> pure []))
  <|> (:) <$> plain <*> template

-- | Run the templateParser on input text, force partial results to terminate with Failure
parse :: (I.ShopifyTemplateSuper e t, I.ShopifyExpressionSuper e) => Text -> Either Text.Text [I.Template (I.Expression e) t]
parse t =
  case feed (Attoparsec.parse template t) Text.empty of
    Fail _ ctxs msg -> Left $ Text.pack $ mconcat $ intersperse ", " $  msg:ctxs
    Partial _ -> Left "not enough inputs"
    Done "" r -> Right r
    Done _ _ -> Left "unparsed inputs remaining"
