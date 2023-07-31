{-# LANGUAGE FlexibleInstances #-}

module SLED.Test
  ( runTestChecks

    -- * Fixtures
  , lts1818
  , freckleApp1011
  , freckleApp1012
  , yesodFlowRoutesGitHub

    -- * Re-exports
  , module X
  ) where

import SLED.Prelude

import Blammo.Logging.Logger (newTestLogger)
import qualified Data.Map.Strict as Map
import SLED.Checks as X
import SLED.ExtraDep as X
import SLED.GitExtraDep
import SLED.Hackage
import SLED.HackageExtraDep
import SLED.PackageName as X
import SLED.Run (runChecks)
import SLED.Stackage
import SLED.StackageResolver as X
import SLED.Suggestion as X
import SLED.Version
import Test.Hspec as X

newtype TestAppT app m a = TestAppT
  { unTestAppT :: ReaderT app (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadLoggerIO
    , MonadReader app
    )

instance Monad m => MonadHackage (TestAppT TestApp m) where
  getHackageVersions package = do
    m <- asks taHackageVersionsByPackage
    pure $ Map.lookup package m

instance Monad m => MonadStackage (TestAppT TestApp m) where
  getStackageVersions resolver package = do
    ms <- asks taStackageVersionsByResolver

    pure $ do
      m <- Map.lookup resolver ms
      Map.lookup package m

runTestAppT
  :: (MonadUnliftIO m, HasLogger app) => TestAppT app m a -> app -> m a
runTestAppT action app =
  runLoggerLoggingT app $ runReaderT (unTestAppT action) app

data TestApp = TestApp
  { taLogger :: Logger
  , taHackageVersionsByPackage :: Map PackageName HackageVersions
  , taStackageVersionsByResolver
      :: Map StackageResolver (Map PackageName StackageVersions)
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

runTestChecks
  :: MonadUnliftIO m
  => Map PackageName HackageVersions
  -> Map StackageResolver (Map PackageName StackageVersions)
  -> StackageResolver
  -> ChecksName
  -> ExtraDep
  -> m [Suggestion]
runTestChecks mockHackage mockStackage resolver checksName extraDep = do
  testApp <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure mockHackage
      <*> pure mockStackage

  runTestAppT (runChecks resolver checksName extraDep) testApp

lts1818 :: StackageResolver
lts1818 = StackageResolver "lts-18.18"

freckleApp1011 :: ExtraDep
freckleApp1011 =
  Hackage
    HackageExtraDep
      { hedPackage = PackageName "freckle-app"
      , hedVersion = parseVersion "1.0.1.1"
      , hedChecksum = Nothing
      }

freckleApp1012 :: ExtraDep
freckleApp1012 =
  Hackage
    HackageExtraDep
      { hedPackage = PackageName "freckle-app"
      , hedVersion = parseVersion "1.0.1.2"
      , hedChecksum = Nothing
      }

yesodFlowRoutesGitHub :: ExtraDep
yesodFlowRoutesGitHub =
  Git
    GitExtraDep
      { gedRepository = Repository "https://github.com/freckle/yesod-routes-flow"
      , gedCommit = CommitSHA "2a9cd873880956dd9a0999b593022d3c746324e8"
      }
