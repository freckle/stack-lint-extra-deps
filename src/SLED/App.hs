module SLED.App
  ( App (..)
  , withApp
  ) where

import SLED.Prelude

import SLED.Options

data App = App
  { appLogFunc :: LogFunc
  , appOptions :: Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc $ \x y -> x {appLogFunc = y}

withApp :: MonadUnliftIO m => Options -> (App -> m b) -> m b
withApp opts f = do
  lo <- optionsLogOptions opts stdout
  withLogFunc lo $ \lf -> do
    f $ App lf opts
