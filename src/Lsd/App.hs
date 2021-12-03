module Lsd.App
    ( App(..)
    , withApp
    ) where

import RIO

import Lsd.Cache
import Lsd.Options
import RIO.Process

data App = App
    { appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appOptions :: Options
    }

optionsL :: Lens' App Options
optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
    processContextL =
        lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasCache App where
    cacheEnabledL = optionsL . cacheEnabledL
    cacheDirectoryL = optionsL . cacheDirectoryL

withApp :: MonadUnliftIO m => Options -> (App -> m b) -> m b
withApp opts f = do
    lo <- optionsLogOptions opts stdout
    withLogFunc lo $ \lf -> do
        app <- App lf <$> mkDefaultProcessContext <*> pure opts

        f app
