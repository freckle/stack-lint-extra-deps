{-# LANGUAGE DerivingVia #-}

module SLED.Options
  ( Options (..)
  , optionsParserInfo
  ) where

import SLED.Prelude

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Options.Applicative
import SLED.Checks
import SLED.StackageResolver
import SLED.Suggestion.Format
import System.FilePath.Glob

data Options = Options
  { path :: Last FilePath
  , resolver :: Last StackageResolver
  , format :: Last Format
  , excludes :: [Pattern]
  , checks :: Maybe ChecksName
  , noExit :: Any
  , filter :: Last Pattern
  , version :: Any
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Options

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
    $ fullDesc
    <> progDesc "stack lint-extra-deps (sled)"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> ( Last
            <$> optional
              ( strOption
                  ( short 'p'
                      <> long "path"
                      <> metavar "PATH"
                      <> help "Path to config to lint"
                  )
              )
        )
    <*> ( Last
            <$> optional
              ( readStackageResolver
                  <$> strOption
                    ( short 'r'
                        <> long "resolver"
                        <> metavar "RESOLVER"
                        <> help "Resolver to use, default is read from --path"
                    )
              )
        )
    <*> ( Last
            <$> optional
              ( option
                  (eitherReader readFormat)
                  ( short 'f'
                      <> long "format"
                      <> metavar "tty|gha|json"
                      <> help "Format to output in"
                  )
              )
        )
    <*> many
      ( strOption
          ( long "exclude"
              <> help "Exclude deps matching PATTERN"
              <> metavar "PATTERN"
          )
      )
    <*> optional checksNameOption
    <*> ( Any
            <$> switch
              ( short 'n'
                  <> long "no-exit"
                  <> help "Exit successfully, even if suggestions found"
              )
        )
    <*> ( Last
            <$> optional
              ( argument
                  str
                  ( metavar "PATTERN"
                      <> help "Limit to deps matching PATTERN"
                  )
              )
        )
    <*> ( Any
            <$> switch
              (long "version" <> help "Print version number information and quit")
        )
