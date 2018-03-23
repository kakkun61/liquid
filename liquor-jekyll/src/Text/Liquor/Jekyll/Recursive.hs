{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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

aggregate :: Aggregate s => Template e s -> [Text]
aggregate = nub . foldl' (flip $ foldStatement aggregateAlgebra) []

type TemplateTuple e s = (Template e s, Context, [FilePath], Maybe FilePath)

loadAndParseAndInterpret
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, Evaluate e, Interpret s, MonadThrow m)
  => Context
  -> FilePath
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m Text
loadAndParseAndInterpret context filePath loader parser = do
  deps <- loadAndParseRecursively filePath loader parser
  case interpretRecursively context deps filePath of
    Left err -> throwM $ LiquidJekyllException err
    Right text -> pure text

loadAndParseAndInterpret'
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, Evaluate e, Interpret s, MonadThrow m)
  => Context
  -> FilePath
  -> Text
  -> Maybe FilePath
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m Text
loadAndParseAndInterpret' context filePath source maybeLayout loader parser = do
  case parser source of
    Left err -> throwM $ LiquidJekyllException err
    Right template -> do
      let
        filePath' = FilePath.normalise filePath
        dependencies = FilePath.normalise . Text.unpack <$> aggregate template
        rest =
          case maybeLayout of
            Just layout -> FilePath.normalise layout : dependencies -- TODO: layout のファイルのルートディレクトリーを設定ファイルで指定できるようにする
            Nothing -> dependencies
        acc = HashMap.fromList [(filePath', (template, context, dependencies, maybeLayout))]
      tuples <- loadAndParseRecursively' rest acc loader parser
      case interpretRecursively context tuples filePath' of
        Left err -> throwM $ LiquidJekyllException err
        Right text -> pure text

loadAndParseRecursively
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, MonadThrow m)
  => FilePath
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m (HashMap FilePath (TemplateTuple e s))
loadAndParseRecursively filePath = loadAndParseRecursively' [filePath] HashMap.empty

-- width first search
loadAndParseRecursively'
  :: (JekyllStatementSuper e s, ShopifyExpressionSuper e, Aggregate s, MonadThrow m)
  => [FilePath]
  -> HashMap FilePath (TemplateTuple e s)
  -> (FilePath -> m (Text, Context))
  -> (Text -> Result (Template e s))
  -> m (HashMap FilePath (TemplateTuple e s))
loadAndParseRecursively' (filePath:r) acc loader parser = do
  if HashMap.member filePath acc
    then
      loadAndParseRecursively' r acc loader parser
    else do
      (source, context) <- loader filePath
      case parser source of
        Left err -> throwM $ LiquidJekyllException err
        Right template -> do
          let
            dependencies = FilePath.normalise . Text.unpack <$> aggregate template -- TODO: include するファイルのルートディレクトリーを設定ファイルで指定できるようにする
            (maybeLayout, filePaths) =
              case HashMap.lookup "layout" context of
                Just (Aeson.String layout) ->
                  let layout' = FilePath.normalise $ Text.unpack layout -- TODO: layout のファイルのルートディレクトリーを設定ファイルで指定できるようにする
                  in (Just layout', layout' : r <> dependencies)
                _ -> (Nothing, r <> dependencies)
            acc' = HashMap.insert filePath (template, context, dependencies, maybeLayout) acc
          loadAndParseRecursively' filePaths acc' loader parser
loadAndParseRecursively' [] acc _ _ = pure acc

-- depth first search
interpretRecursively
  :: (Evaluate e, Interpret s)
  => Context
  -> HashMap FilePath (TemplateTuple e s)
  -> FilePath
  -> Result Text
interpretRecursively context tuples filePath = do
  let filePath' = FilePath.normalise filePath
  rs <- interpretRecursively' context HashMap.empty filePath' filePath' Nothing
  case HashMap.lookup filePath rs of
    Just t -> Right t
    Nothing -> Left "interpreting failed: code error"
  where
    interpretRecursively'
       :: Context
       -> HashMap FilePath Text
       -> FilePath
       -> FilePath
       -> Maybe Text
       -> Result (HashMap FilePath Text)
    interpretRecursively' context' rs searchFilePath saveFilePath maybeContent = do
      case HashMap.lookup saveFilePath rs of
        Just _ -> Right rs
        Nothing ->
          case HashMap.lookup searchFilePath tuples of
            Nothing -> Left $ "parsed template not found: " <> Text.pack searchFilePath
            Just (template, fileContext, [], maybeLayout) -> do
              let c = addContentIfNecessary $ HashMap.union fileContext context'
              content' <- interpret c template
              case maybeLayout of
                Nothing -> pure $ HashMap.insert saveFilePath content' rs
                Just layout -> interpretRecursively' c rs (FilePath.normalise layout) saveFilePath (Just content')
            Just (template, fileContext, deps, maybeLayout) -> do
              let c = HashMap.union fileContext context'
              rs' <- depsLoop c rs deps
              let c' = addContentIfNecessary $ unionContext c rs'
              content' <- interpret c' template
              case maybeLayout of
                Nothing -> pure $ HashMap.insert saveFilePath content' rs
                Just layout -> interpretRecursively' c rs' (FilePath.normalise layout) saveFilePath (Just content')
      where
        depsLoop :: Context -> HashMap FilePath Text -> [FilePath] -> Result (HashMap FilePath Text)
        depsLoop c rs' = foldl' go (Right rs')
          where
            go :: Result (HashMap FilePath Text) -> FilePath -> Result (HashMap FilePath Text)
            go (Left err) _ = Left err
            go (Right rs'') p = interpretRecursively' c rs'' p p Nothing

        addContentIfNecessary :: Context -> Context
        addContentIfNecessary =
          case maybeContent of
            Just content -> HashMap.insert "content" (Aeson.String content)
            Nothing -> id

    unionContext :: Context -> HashMap FilePath Text -> Context
    unionContext = HashMap.foldlWithKey' go
      where
        go :: Context -> FilePath -> Text -> Context
        go c q t = HashMap.insert (variableFilePrefix <> Text.pack q) (Aeson.String t) c

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

instance {-# OVERLAPPING #-} MonadThrow (Either Text) where
  throwM = Left . Text.pack . show
