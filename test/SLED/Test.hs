{-# LANGUAGE FlexibleInstances #-}

module SLED.Test
  ( TestAppT (..)
  , runTestAppT

    -- * Concrete Test App
  , TestApp (..)
  , newTestApp

    -- * Re-exports
  , module X
  ) where

import SLED.Prelude

import Blammo.Logging.Logger

import SLED.StackYaml
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

runTestAppT
  :: (MonadUnliftIO m, HasLogger app) => TestAppT app m a -> app -> m a
runTestAppT action app =
  runLoggerLoggingT app $ runReaderT (unTestAppT action) app

data TestApp = TestApp
  { taLogger :: Logger
  , taStackYaml :: StackYaml
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

newTestApp :: MonadIO m => StackYaml -> m TestApp
newTestApp taStackYaml = do
  taLogger <- newTestLogger defaultLogSettings
  pure TestApp {..}
