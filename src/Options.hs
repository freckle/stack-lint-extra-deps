module Options
  ( Options(..)
  , parseOptions
  , optionsLogOptions
  , ColorOption(..)
  ) where

import RIO

import Checks
import Options.Applicative
import Options.BoundedEnum
import Report
import StackageResolver
import System.Environment (lookupEnv)
import System.FilePath.Glob

data Options = Options
  { oPath :: FilePath
  , oResolver :: Maybe StackageResolver
  , oExcludes :: [Pattern]
  , oChecks :: ChecksName
  , oFormat :: Format
  , oNoExit :: Bool
  , oColor :: ColorOption
  , oVerbose :: Bool
  , oFilter :: Maybe Pattern
  }

optionsLogOptions :: MonadIO m => Options -> Handle -> m LogOptions
optionsLogOptions Options {..} h = do
  useColor <- case oColor of
    ColorAuto -> hIsTerminalDevice h
    ColorAlways -> pure True
    ColorNever -> pure False
  setLogVerboseFormat True
    . setLogUseColor useColor
    . setLogMinLevel minLevel
    <$> logOptionsHandle h False
  where minLevel = if oVerbose then LevelDebug else LevelInfo

parseOptions :: IO Options
parseOptions = do
  envStackYaml <- fromMaybe "stack.yaml" <$> lookupEnv "STACK_YAML"
  execParser $ info (options envStackYaml <**> helper) $ fullDesc <> progDesc
    "Lint Stackage (extra) Deps"

-- brittany-disable-next-binding

options :: FilePath -> Parser Options
options stackYaml = Options
    <$> strOption
        (  short 'p'
        <> long "path"
        <> metavar "PATH"
        <> help "Path to config to lint"
        <> value stackYaml
        <> showDefault
        )
    <*> optional (option (eitherReader stackageResolver)
        (  short 'r'
        <> long "resolver"
        <> metavar "RESOLVER"
        <> help "Resolver to use, default is read from --path"
        ))
    <*> many (strOption
        (  long "exclude"
        <> help "Exclude deps matching PATTERN"
        <> metavar "PATTERN"
        ))
    <*> checksNameOption
    <*> formatOption
    <*> switch
        (  short 'n'
        <> long "no-exit"
        <> help "Exit successfully, even if suggestions found"
        )
    <*> boundedEnumOptionWith showColorOption (\list ->
           short 'c'
        <> long "color"
        <> help ("When to use color, one of: " <> list)
        <> metavar "COLOR"
        <> value ColorAuto
        )
    <*> switch
        (  short 'v'
        <> long "verbose"
        <> help "Log verbosely"
        )
    <*> optional (argument str
        (  metavar "PATTERN"
        <> help "Limit to deps matching PATTERN"
        ))

data ColorOption
    = ColorAuto
    | ColorAlways
    | ColorNever
    deriving (Bounded, Enum)

showColorOption :: ColorOption -> String
showColorOption = \case
  ColorAuto -> "auto"
  ColorAlways -> "always"
  ColorNever -> "never"
