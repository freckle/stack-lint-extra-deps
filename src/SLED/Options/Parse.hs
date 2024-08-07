module SLED.Options.Parse
  ( Options (..)
  , PrintVersion (..)
  , parseOptions
  ) where

import SLED.Prelude

import Data.Yaml.Marked.Decode
import Options.Applicative
import SLED.Checks
import SLED.Options (optionsParserInfo)
import qualified SLED.Options as Types
import SLED.Options.Pragma (parsePragmaOptions)
import SLED.StackYaml
import SLED.StackageResolver
import SLED.Suggestion.Format
import System.FilePath.Glob
import System.IO (hPutStrLn)

data Options = Options
  { path :: FilePath
  , resolver :: Marked StackageResolver
  , format :: Format
  , excludes :: [Pattern]
  , checks :: ChecksName
  , noExit :: Bool
  , filter :: Maybe Pattern
  , stackYaml :: StackYaml
  , stackYamlContents :: ByteString
  }

data PrintVersion = PrintVersion

parseOptions :: IO (Either PrintVersion Options)
parseOptions = do
  cli <- execParser optionsParserInfo
  if getAny cli.version
    then pure $ Left PrintVersion
    else do
      envStackYaml <- lookupEnv "STACK_YAML"

      let path = fromMaybe "stack.yaml" $ getLast cli.path <|> envStackYaml

      bs <- readFileBS path
      stackYaml <- liftIO $ markedItem <$> decodeThrow decodeStackYaml path bs

      let (errs, pragmas) = parsePragmaOptions bs

      for_ errs $ hPutStrLn stderr . ("Warning: invalid @sled pragma:\n" <>)

      let
        options = pragmas <> cli

        -- Mark an option resolver with the in-file resolver's position so that if
        -- we do anything based on it, that's what we'll use
        resolver = maybe stackYaml.resolver (<$ stackYaml.resolver) $ getLast options.resolver

      pure
        $ Right
          Options
            { path = path
            , resolver = resolver
            , format = fromMaybe defaultFormat $ getLast options.format
            , excludes = options.excludes
            , checks = fromMaybe AllChecks $ options.checks
            , noExit = getAny options.noExit
            , filter = getLast options.filter
            , stackYaml = stackYaml
            , stackYamlContents = bs
            }
