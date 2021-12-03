module Lsd.Options
    ( Options(..)
    , parseOptions
    , optionsLogOptions
    , ColorOption(..)
    ) where

import RIO

import Lsd.Cache
import Lsd.Checks
import Lsd.Report
import Lsd.StackageResolver
import Options.Applicative
import System.Environment (lookupEnv)
import System.FilePath.Glob

data Options = Options
    { oResolver :: Maybe StackageResolver
    , oExcludes :: [Pattern]
    , oChecks :: ChecksName
    , oFormat :: Format
    , oNoExit :: Bool
    , oCacheDirectory :: FilePath
    , oNoCache :: Bool
    , oColor :: ColorOption
    , oVerbose :: Bool
    , oPath :: FilePath
    }

instance HasCache Options where
    cacheEnabledL = lens (not . oNoCache) $ \x y -> x { oNoCache = not y }
    cacheDirectoryL = lens oCacheDirectory $ \x y -> x { oCacheDirectory = y }

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
    envCacheDir <- defaultCacheDirectory
    execParser
        $ info (options envStackYaml envCacheDir <**> helper)
        $ fullDesc
        <> progDesc "Lint Stackage (extra) Deps"

-- brittany-disable-next-binding

options :: FilePath -> FilePath -> Parser Options
options stackYaml cacheDir = Options
    <$> optional (option (eitherReader stackageResolver)
        (  short 'r'
        <> long "resolver"
        <> help "Override resolver from stack.yaml"
        ))
    <*> many (strOption
        (  long "exclude"
        <> help "Exclude deps by glob"
        ))
    <*> option (eitherReader readChecksName)
        (  long "checks"
        <> help ("Checks to run, one of " <> checksNameList)
        <> value AllChecks
        )
    <*> option (eitherReader readFormat)
        (  short 'f'
        <> long "format"
        <> help ("Output format, one of " <> formatList)
        <> value Detailed
        )
    <*> switch
        (  long "no-exit"
        <> help "Exit successfully even if suggestions found"
        )
    <*> strOption
        (  long "cache-dir"
        <> help "Overide cache directory, default follows XDG"
        <> value cacheDir
        <> showDefault
        )
    <*> switch
        (  long "no-cache"
        <> help "Ignore caches"
        )
    <*> option (eitherReader readColorOption)
        (  short 'c'
        <> long "color"
        <> help "When to use color: auto, always, never"
        <> value ColorAuto
        )
    <*> switch
        (  short 'v'
        <> long "verbose"
        <> help "Log verbosely"
        )
    <*> argument str
        (  metavar "PATH"
        <> help "Path to config to lint"
        <> value stackYaml
        <> showDefault
        )

data ColorOption
    = ColorAuto
    | ColorAlways
    | ColorNever

readColorOption :: String -> Either String ColorOption
readColorOption = \case
    "auto" -> Right ColorAuto
    "always" -> Right ColorAlways
    "never" -> Right ColorNever
    x -> Left $ "Invalid color option: " <> x
