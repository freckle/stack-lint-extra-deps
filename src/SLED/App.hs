module SLED.App
  ( AppT (..)
  , runAppT

    -- * Our concrete App
  , App (..)
  ) where

import SLED.Prelude

import qualified Data.Yaml as Yaml
import SLED.Options
import SLED.StackYaml

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

instance MonadIO m => MonadStackYaml (AppT app m) where
  loadStackYaml path = do
    logDebug $ "Loading stack.yaml" :# ["path" .= path]
    Yaml.decodeFileThrow path

runAppT :: (MonadUnliftIO m, HasLogger app) => AppT app m a -> app -> m a
runAppT action app =
  runLoggerLoggingT app $ runReaderT (unAppT action) app

data App = App
  { appOptions :: Options
  , appLogger :: Logger
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}
