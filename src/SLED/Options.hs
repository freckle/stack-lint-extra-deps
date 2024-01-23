module SLED.Options
  ( Options (..)
  , parseOptions
  ) where

import SLED.Prelude

import Options.Applicative
import SLED.Checks
import SLED.StackageResolver
import System.FilePath.Glob

data Options = Options
  { path :: FilePath
  , resolver :: Maybe StackageResolver
  , excludes :: [Pattern]
  , checks :: ChecksName
  , noExit :: Bool
  , filter :: Maybe Pattern
  }

parseOptions :: IO Options
parseOptions = do
  envStackYaml <- fromMaybe "stack.yaml" <$> lookupEnv "STACK_YAML"
  execParser
    $ info (options envStackYaml <**> helper)
    $ fullDesc
    <> progDesc
      "Lint Stackage (extra) Deps"

options :: FilePath -> Parser Options
options stackYaml =
  Options
    <$> strOption
      ( short 'p'
          <> long "path"
          <> metavar "PATH"
          <> help "Path to config to lint"
          <> value stackYaml
          <> showDefault
      )
    <*> optional
      ( StackageResolver
          <$> strOption
            ( short 'r'
                <> long "resolver"
                <> metavar "RESOLVER"
                <> help "Resolver to use, default is read from --path"
            )
      )
    <*> many
      ( strOption
          ( long "exclude"
              <> help "Exclude deps matching PATTERN"
              <> metavar "PATTERN"
          )
      )
    <*> checksNameOption
    <*> switch
      ( short 'n'
          <> long "no-exit"
          <> help "Exit successfully, even if suggestions found"
      )
    <*> optional
      ( argument
          str
          ( metavar "PATTERN"
              <> help "Limit to deps matching PATTERN"
          )
      )
