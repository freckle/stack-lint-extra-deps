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

import qualified Data.Map.Strict as Map
import SLED.Hackage
import SLED.PackageName
import SLED.Stackage
import SLED.StackageResolver
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
