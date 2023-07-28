{-# LANGUAGE FlexibleInstances #-}

module SLED.Test
  ( TestAppT (..)
  , runTestAppT

    -- * Concrete Test App
  , TestApp (..)

    -- * Re-exports
  , module X
  ) where

import SLED.Prelude

import SLED.Hackage
import SLED.PackageName
import SLED.StackYaml
import SLED.Stackage
import SLED.StackageResolver
import Test.Hspec as X (Spec, describe, example, it)
import Test.Hspec.Expectations.Lifted as X

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

instance Monad m => MonadStackYaml (TestAppT TestApp m) where
  loadStackYaml _path = asks taStackYaml

instance Monad m => MonadHackage (TestAppT TestApp m) where
  getHackageVersions package = do
    f <- asks taGetHackageVersions
    pure $ f package

instance Monad m => MonadStackage (TestAppT TestApp m) where
  getStackageVersions resolver package = do
    f <- asks taGetStackageVersions
    pure $ f resolver package

runTestAppT
  :: (MonadUnliftIO m, HasLogger app) => TestAppT app m a -> app -> m a
runTestAppT action app =
  runLoggerLoggingT app $ runReaderT (unTestAppT action) app

data TestApp = TestApp
  { taLogger :: Logger
  , taStackYaml :: StackYaml
  , taGetHackageVersions :: PackageName -> Maybe HackageVersions
  , taGetStackageVersions
      :: StackageResolver
      -> PackageName
      -> Maybe StackageVersions
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}
