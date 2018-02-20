{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Liquoh.Parser where

import Prelude hiding (and, null, or, takeWhile)

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Expr
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Scientific (toBoundedInteger)
import Data.Semigroup hiding (option)
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace

import Text.Liquoh.Interpreter hiding (variable, nil, output, plain, if_)
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

elsIf :: Parser Text
elsIf = "elsif"

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
varIndexSection :: Parser VariablePath
varIndexSection = do
  vs <- sepBy var dot
  i  <- many parseBoxedInt
  brokenChar <- openBracket <|> return '~'
  let ixs = do obs <- (nonEmpty (ObjectKey <$> vs))
               Just obs <> (nonEmpty (ArrayKey <$> i))
  if brokenChar == '[' then (fail "invalid array index - ill-typed") else case ixs of
    Just nel -> return nel
    Nothing  -> fail "invalid var index section"

-- | Parse a variable
variable :: Variable :<: e => Parser (Expression e)
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
plain :: Plain :<: t => Parser (Template (Expression e) t)
plain = traceParserLabel "plain" $ I.plain <$> (mapText $ manyTill1 (traceParser "plain body" anyChar) terminator)
  where terminator = traceParser "plain terminator" $ lookAhead $ tagStart <|> outputStart <|> (endOfInput *> pure Text.empty)

-- | Force the first character to be valid, otherwise fail miserably
manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p e = (:) <$> p <*> s
    where s = (e *> pure []) <|> ((:) <$> p <*> s)

-- | Match a quoted string
quoteString :: Value :<: e => Parser (Expression e)
quoteString = do
  skipSpace
  beginTick <- satisfy (inClass "\'\"")
  qs        <- mapText $ manyTill anyChar (char beginTick)
  return $ I.string qs

expression
  :: ShopifyExpressionSuper e
  => Parser (Expression e)
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
  :: (Equal :<: e, NotEqual :<: e, GraterEqual :<: e, LessEqual :<: e, Grater :<: e, Less :<: e)
  => Parser (Expression e -> Expression e -> Expression e)
comparisonOperator =
  stripped $ eq *> pure (.==.) <|>
             neq *> pure (./=.) <|>
             gtEq *> pure (.>=.) <|>
             ltEq *> pure (.<=.) <|>
             (mapText . some $ gt) *> pure (.>.) <|>
             (mapText . some $ lt) *> pure (.<.) {- <|>
             contains *> pure I.contains -}

andOperator :: And :<: e => Parser (Expression e -> Expression e -> Expression e)
andOperator = stripped $ and *> pure (.&&.)

orOperator :: And :<: e => Parser (Expression e -> Expression e -> Expression e)
orOperator = stripped $ or *> pure (.&&.)

--arrayOperator ::
--arrayOperator = stripped $ comma *> pure ()

term :: (Value :<: e, Variable :<: e) => Parser (Expression e)
term = immediateValue <|> variable

-- | Match a immediate value
immediateValue :: Value :<: e => Parser (Expression e)
immediateValue =
  quoteString <|>
  (I.number <$> scientific) <|>
  I.nil <$ (stripped nil) <|>
  bool False <$ (stripped false) <|>
  bool True <$ (stripped true)

---- | Parse and evaluate truthiness
--truthy :: Parser Expr
--truthy =
--  Null    <$ (stripped null)              <|>
--  Nil     <$ (stripped nil)               <|>
--  Falseth <$ (stripped false)             <|>
--  Trueth  <$ (stripped true)              <|>
--  (Truthy . Num <$> stripped scientific)  <|>
--  (Truthy       <$> stripped quoteString) <|>
--  (Truthy       <$> stripped variable)

if_ :: (ShopifyTemplateSuper e t, ShopifyExpressionSuper e) => Parser [Template (Expression e) t] -> Parser (Template (Expression e) t)
if_ nest = traceParserLabel "if" $ do
  c <- tagWith ifStart expression
  t <- nest
  _ <- tag ifEnd
  return $ I.if_ [(c, t)]

---- | Match any predicate clause
--predicateClause :: Parser Expr
--predicateClause =
--  predicate <|> binaryPredicate <|> truthy
--
---- | Match an if clause
--ifClause :: Parser Expr
--ifClause = IfClause <$> tagWith ifStart predicateClause
--
---- | Match an ifkey clause
--ifKeyClause :: Parser Expr
--ifKeyClause = IfKeyClause <$> tagWith ifKeyStart variableOnly
--  where variableOnly = do res <- eitherP precheck variable
--                          case res of
--                            Left _   -> fail "Only variables as ifkey args allowed"
--                            Right ok -> return ok
--        precheck     = Null    <$ (stripped null)             <|>
--                       Nil     <$ (stripped nil)              <|>
--                       Falseth <$ (stripped false)            <|>
--                       Trueth  <$ (stripped true)             <|>
--                       (Truthy . Num <$> stripped scientific) <|>
--                       (Truthy       <$> stripped quoteString)
--
---- | Match an elsif clause
--elsifClause :: Parser Expr
--elsifClause = ElsIfClause <$> tagWith elsIf predicateClause
--
---- | Match an else clause
--elseClause :: Parser Expr
--elseClause = (tag els) *> pure Else
--
---- | Match the end of an if clause
--endIfClause :: Parser Expr
--endIfClause = (tag endIf) *> pure Noop
--
---- | Match a variable condition for a case clause
--caseClause :: Parser Expr
--caseClause = tagWith caseStart variable
--
---- | Match a when clause, part of a case pattern match block
--whenClause :: Parser Expr
--whenClause = tagWith when (quoteString <|> (Num <$> scientific))
--
---- | Match the end of a case pattern match block
--endCaseClause :: Parser Expr
--endCaseClause = (tag caseEnd) *> pure Noop
--
---- | Match an assign clause
--assignClause :: Parser Expr
--assignClause = do
--  (var, val) <- tagWith assign assignExpr
--  pure $ AssignClause var val
--  where
--    assignExpr :: Parser (Expr, Expr)
--    assignExpr = do
--      var <- variable
--      _ <- skipSpace *> eqSign
--      val <- skipSpace *> valueExpression
--      pure (var, val)
--
---- | Match a for clause
--forClause :: Parser Expr
--forClause = do
--  (elmVar, arrVar) <- tagWith forStart inClause
--  return $ ForClause elmVar arrVar
--  where
--    inClause :: Parser (Expr, Expr)
--    inClause = do
--      elmVar <- skipSpace *> variable
--      _ <- skipSpace *> iN
--      arrVar <- skipSpace *> variable
--      return (elmVar, arrVar)
--
---- | Match the end of an for clause
--endForClause :: Parser Expr
--endForClause = (tag forEnd) *> pure Noop
--
---- | Match a filter fn name
--filterName :: Parser Text
--filterName = mapText $ skipSpace *> manyTill1 letter terminator
--  where terminator = colon              <|>
--                    (skipSpace *> pipe) <|>
--                    (lookAhead $ satisfy (not . isAlpha))
--
---- | Match the list of arguments for the filter fn
--filterArgs :: Parser [Expr]
--filterArgs = skipSpace *> sepBy numOrString comma
--  where numOrString = skipSpace *> (Num <$> scientific) <|> quoteString
--
---- | Match a filter cell, fn and args
--filterCell :: Parser Expr
--filterCell = do
--  fnName <- filterName
--  args   <- filterArgs
--  typeCheckFilter fnName args
--
---- | Type check the function args and check arity
--typeCheckFilter :: Text
--                -> [Expr]
--                -> Parser Expr
--typeCheckFilter "toUpper"             []                                   =
--  return $ FilterCell "toUpper" []
--typeCheckFilter "toUpper"             _                                    =
--  fail "toUpper filter takes no arguments"
--typeCheckFilter "toLower"             []                                   =
--  return $ FilterCell "toLower" []
--typeCheckFilter "toLower"             _                                    =
--  fail "toLower filter takes no arguments"
--typeCheckFilter "toTitle"             []                                   =
--  return $ FilterCell "toTitle" []
--typeCheckFilter "toTitle"             _                                    =
--  fail "toTitle filter takes no arguments"
--typeCheckFilter "replace"             a@[QuoteString _, QuoteString _]     =
--  return $ FilterCell "replace"       a
--typeCheckFilter "replace"             _                                    =
--  fail "replace filter requires find, replace strings as args"
--typeCheckFilter "first"               []                                   =
--  return $ FilterCell "first" []
--typeCheckFilter "first"               _                                    =
--  fail "first filter takes no arguments"
--typeCheckFilter "firstOrDefault"      a@(_:_)                              =
--  return $ FilterCell "firstOrDefault" a
--typeCheckFilter "firstOrDefault" _                                         =
--  fail "firstOrDefault requires a single default parameter"
--typeCheckFilter "last"                []                                   =
--  return $ FilterCell "last" []
--typeCheckFilter "last"           _                                         =
--  fail "last filter takes no arguments"
--typeCheckFilter "lastOrDefault"       a@(_:_)                              =
--  return $ FilterCell "lastOrDefault" a
--typeCheckFilter "lastOrDefault"       _                                    =
--  fail "lastOrDefault requires a single default parameter"
--typeCheckFilter "countElements"       []                                   =
--  return $ FilterCell "countElements" []
--typeCheckFilter "countElements"       _                                    =
--  fail "countElements takes no arguments"
--typeCheckFilter "renderWithSeparator" a@[QuoteString _]                    =
--  return $ FilterCell "renderWithSeparator" a
--typeCheckFilter "renderWithSeparator" _                                    =
--  fail "renderWithSeparator requires a separator argument with which to intersperse the target array"
--typeCheckFilter "toSentenceWithSeparator" a@[QuoteString _, QuoteString _] =
--  return $ FilterCell "toSentenceWithSeparator" a
--typeCheckFilter "toSentenceWithSeparator" _                                =
--  fail "toSentenceWithSeparator requires a separator argument and last element separator"
--typeCheckFilter l                     _                                    =
--  fail $ (show l) ++ ": function isn't supported"
--
---- | Match multiple filter fns and args
--filterCells :: Parser [Expr]
--filterCells = many ((filterCell <* (skipSpace *> pipe)) <|> filterCell)
--
---- | Match a lhs and a block of filters with their args
--filterBlock :: Parser Expr
--filterBlock = do
--  lhs   <- (quoteString <|> (skipSpace *> variable)) <* (skipSpace >> pipe)
--  cells <- filterCells
--  return $ Filter lhs cells

-- | Output block, a variable, indexed variable, number or filter block
output :: (Output e :<: t, ShopifyExpressionSuper e) => Parser (Template (Expression e) t)
output = traceParserLabel "output" $ I.output <$> outputTag ({-filterBlock <|>-} expression)

---- | If statement, optional elsif or else
--ifLogic :: Parser Expr
--ifLogic = do
--  start  <- ifClause <|> ifKeyClause <|> elsifClause <|> elseClause
--  iftrue <- TrueStatements <$>
--            manyTill (output <|> textPart)
--                     (lookAhead elsifClause <|>
--                      lookAhead elseClause  <|>
--                      lookAhead endIfClause)
--  let sofar = IfLogic start iftrue
--  (endIfClause *> pure sofar) <|> (IfLogic sofar <$> ifLogic)
--
---- | Case pattern match block
--caseLogic :: Parser Expr
--caseLogic = do
--    start    <- caseClause
--    patterns <- many1 whenBlock
--    _        <- endCaseClause
--    return $ CaseLogic start patterns
--  where whenBlock = do
--          pattern <- whenClause <|> elseClause
--          iftrue  <- TrueStatements <$>
--                     manyTill (output <|> textPart)
--                              (lookAhead whenClause <|>
--                               lookAhead elseClause <|>
--                               lookAhead endCaseClause)
--          return (pattern, iftrue)
--
---- | For iteration
--forLogic :: Parser Expr
--forLogic = do
--  start <- forClause
--  iteration <-
--    TrueStatements <$>
--      manyTill (output <|> textPart) endForClause
--  return $ ForLogic start iteration
--
-- | Parse any block type
tagBlock :: (ShopifyTemplateSuper e t, ShopifyExpressionSuper e) => Parser (Template (Expression e) t)
tagBlock = traceParserLabel "tag block" (
  choice
    [ output
    , if_ template
    ] <?> "Block Parsing")

-- | Parse an entire template into chunks
template :: (ShopifyTemplateSuper e t, ShopifyExpressionSuper e) => Parser [Template (Expression e) t]
template = traceParserLabel "template" (
  traceParserLabel "template: end of input" (endOfInput *> pure [])
  <|> traceParserLabel "template: tag block <> template" (do
    e <- tagBlock
    t <- template
    pure $ e:t)
  <|> traceParserLabel "template: plain <> ((tag block) <> template <|> end of input)" (do
    p <- plain
    r <-
      do
        b <- tagBlock
        r <- template
        pure $ b:r
      <|> pure []
    pure $ p:r))


---- | Run the templateParser on input text, force partial results to terminate with Failure
--parseTemplate :: Text
--              -> IResult Text [Expr]
--parseTemplate t =
--  feed (parse templateParser t) Text.empty
--
--templateP :: Prism' Text [Expr]
--templateP = prism' back forw
--  where forw = maybeResult . parseTemplate
--        back = mconcat . fmap renderExpr

traceParser :: Show a => String -> Parser a -> Parser a
traceParser label p = do
  a <- lookAhead p
  trace (label <> ": " <> show a) p

traceParserLabel :: String -> Parser a -> Parser a
traceParserLabel label p = trace label p
