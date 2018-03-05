{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Text.Liquor.Jekyll.Recursive where

import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import Data.List (nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import System.FilePath (FilePath, (</>))
import qualified System.FilePath as FilePath

import Text.Liquor.Common
import Text.Liquor.Interpreter
import Text.Liquor.Interpreter.Common
import Text.Liquor.Interpreter.Expression
import Text.Liquor.Interpreter.Statement
import Text.Liquor.Jekyll.Common
import Text.Liquor.Jekyll.Interpreter
import Text.Liquor.Jekyll.Interpreter.Statement

class Functor f => Aggregate f where
  aggregateAlgebra :: f ([Text] -> [Text]) -> [Text] -> [Text]

instance (Aggregate f, Aggregate g) => Aggregate (f :+: g) where
  aggregateAlgebra (InjectLeft x) = aggregateAlgebra x
  aggregateAlgebra (InjectRight x) = aggregateAlgebra x

instance Aggregate Plain where
  aggregateAlgebra _ = id

instance Aggregate (Output e) where
  aggregateAlgebra _ = id

instance Aggregate (If e) where
  aggregateAlgebra (If ((_, ts):r)) ps = aggregateAlgebra (If r) (aggregateStatements ts ps)
  aggregateAlgebra (If []) ps = ps

instance Aggregate (Case e) where
  aggregateAlgebra (Case e ((_, ts):r)) ps = aggregateAlgebra (Case e r) (aggregateStatements ts ps)
  aggregateAlgebra (Case _ []) ps = ps

instance Aggregate (For e) where
  aggregateAlgebra (For _ _ ts) = aggregateStatements ts

instance Aggregate (Assign e) where
  aggregateAlgebra _ = id

instance Aggregate Include where
  aggregateAlgebra (Include p) ps = p:ps

aggregateStatements :: [[Text] -> [Text]] -> [Text] -> [Text]
aggregateStatements ts ps = foldl' (flip ($)) ps ts

aggregate :: Aggregate s => [Statement e s] -> [Text]
aggregate = nub . foldl' (flip $ foldStatement aggregateAlgebra) []

loadAndParseAndInterpret
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, Evaluate e, Interpret s, MonadThrow m)
  => Context
  -> FilePath
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m Text
loadAndParseAndInterpret context filePath loader parser = do
  ds <- loadAndParseRecursively filePath loader parser
  case interpretRecursively context ds filePath of
    Left err -> throwM $ LiquidJekyllException err
    Right text -> pure text

-- width first search
loadAndParseRecursively
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, MonadThrow m)
  => FilePath
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m (HashMap FilePath (Template e s, Context, [FilePath]))
loadAndParseRecursively filePath = loadAndParseRecursively' [filePath] HashMap.empty
  where
    loadAndParseRecursively'
      :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, MonadThrow m)
      => [FilePath]
      -> HashMap FilePath (Template e s, Context, [FilePath])
      -> (FilePath -> m (Text, Context))
      -> (Text -> Result (Template e s))
      -> m (HashMap FilePath (Template e s, Context, [FilePath]))
    loadAndParseRecursively' (filePath':r) acc loader parser = do
      if HashMap.member filePath' acc
        then
          loadAndParseRecursively' r acc loader parser
        else do
          (source, context) <- loader filePath'
          case parser source of
            Left err -> throwM $ LiquidJekyllException err
            Right template -> do
              let
                dependencies = (FilePath.takeDirectory filePath' </>) . Text.unpack <$> aggregate template
                acc' = HashMap.insert filePath' (template, context, dependencies) acc
              loadAndParseRecursively' (r <> dependencies) acc' loader parser
    loadAndParseRecursively' [] acc _ _ = pure acc

-- depth first search
interpretRecursively
  :: (Evaluate e, Interpret s)
  => Context
  -> HashMap FilePath (Template e s, Context, [FilePath])
  -> FilePath
  -> Result Text
interpretRecursively context dependencies filePath = do
  rs <- interpretRecursively' dependencies HashMap.empty filePath
  case HashMap.lookup filePath rs of
    Just t -> Right t
    Nothing -> Left "interpreting failed: code error"
  where
    interpretRecursively'
       :: (Evaluate e, Interpret s)
       => HashMap FilePath (Template e s, Context, [FilePath])
       -> HashMap FilePath Text
       -> FilePath
       -> Result (HashMap FilePath Text)
    interpretRecursively' ds rs p =
      case HashMap.lookup p rs of
        Just _ -> Right rs
        Nothing ->
          case HashMap.lookup p ds of
            Nothing -> Left $ "parsed template not found: " <> Text.pack p
            Just (template, context', []) -> do
              t <- interpret (HashMap.union context' context) template
              pure $ HashMap.insert p t rs
            Just (template, context', deps) -> do
              rs' <- depsLoop rs deps
              let c = unionContext p (HashMap.union context' context) rs'
              t <- interpret c template
              pure $ HashMap.insert p t rs
      where
        depsLoop :: HashMap FilePath Text -> [FilePath] -> Result (HashMap FilePath Text)
        depsLoop rs' = foldl' go (Right rs')
          where
            go :: Result (HashMap FilePath Text) -> FilePath -> Result (HashMap FilePath Text)
            go (Left err) _ = Left err
            go (Right rs'') p' = interpretRecursively' ds rs'' p'

    unionContext :: FilePath -> Context ->  HashMap FilePath Text ->Context
    unionContext p = HashMap.foldlWithKey' go
      where
        go :: Context -> FilePath -> Text -> Context
        go c q t = HashMap.insert (variableFilePrefix <> Text.pack (FilePath.makeRelative (FilePath.dropFileName p) q)) (Aeson.String t) c

load :: FilePath -> IO (Text, Context)
load filePath = do
  body <- Text.readFile filePath
  case Text.breakOn separator body of
    ("", r) ->
      let
        r' = Text.drop separatorLength r
      in
        case Text.breakOn separator r' of
          (_, "") -> pure (body, HashMap.empty)
          (ctxTxt, r'') ->
            let
              b = Text.drop separatorLength r''
            in
              case Yaml.decode (Text.encodeUtf8 ctxTxt) of
                Just (Aeson.Object context) ->
                  pure (b, context)
                Just _ -> error "top level of YAML must be object"
                Nothing -> error "failed to parse context YAML"
    _ -> pure (body, HashMap.empty)
  where
    separator :: Text
    separator = "---\n"
    separatorLength :: Int
    separatorLength = 4
