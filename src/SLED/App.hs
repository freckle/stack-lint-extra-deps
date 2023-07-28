module SLED.App
  ( AppT (..)
  , runAppT

    -- * Our concrete App
  , App (..)
  ) where

import SLED.Prelude

import SLED.Options

newtype AppT app m a = AppT
  { unAppT :: ReaderT app (LoggingT m) a
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

runAppT :: (MonadUnliftIO m, HasLogger app) => AppT app m a -> app -> m a
runAppT action app =
  runLoggerLoggingT app $ runReaderT (unAppT action) app

data App = App
  { appOptions :: Options
  , appLogger :: Logger
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}
