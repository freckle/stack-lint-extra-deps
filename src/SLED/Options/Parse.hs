module SLED.Options.Parse
  ( Options (..)
  , PrintVersion (..)
  , parseOptions
  ) where

import SLED.Prelude

import Options.Applicative
import SLED.Checks
import SLED.Options (optionsParserInfo)
import qualified SLED.Options as Types
import SLED.Options.Pragma (parsePragmaOptions)
import SLED.StackageResolver
import SLED.Suggestion.Format
import System.FilePath.Glob
import System.IO (hPutStrLn)

data Options = Options
  { path :: FilePath
  , resolver :: Maybe StackageResolver
  , contents :: ByteString
  , format :: Format
  , excludes :: [Pattern]
  , checkResolver :: Bool
  , checks :: ChecksName
  , noExit :: Bool
  , autoFix :: Bool
  , filter :: Maybe Pattern
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

      let (errs, pragmas) = parsePragmaOptions bs

      for_ errs $ hPutStrLn stderr . ("Warning: invalid @sled pragma:\n" <>)

      let options = pragmas <> cli

      pure
        $ Right
          Options
            { path = path
            , contents = bs
            , resolver = getLast options.resolver
            , format = fromMaybe defaultFormat $ getLast options.format
            , excludes = options.excludes
            , checkResolver = not $ getAny options.noCheckResolver
            , checks = fromMaybe AllChecks $ options.checks
            , noExit = getAny options.noExit
            , autoFix = getAny options.autoFix
            , filter = getLast options.filter
            }
