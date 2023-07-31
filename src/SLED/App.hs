module SLED.App
  ( AppT (..)
  , runAppT

    -- * Our concrete App
  , App (..)
  ) where

import SLED.Prelude

import Control.Error.Util (hush, note)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml as Yaml
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (status200)
import SLED.Hackage
import SLED.Options
import SLED.PackageName
import SLED.StackYaml
import SLED.Stackage
import SLED.StackageResolver

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

instance MonadIO m => MonadHackage (AppT app m) where
  getHackageVersions package = do
    eVersions <-
      httpParse eitherDecode
        . addRequestHeader hAccept "application/json"
        =<< liftIO
          ( parseRequest
              $ unpack
              $ "https://hackage.haskell.org/package/"
              <> unPackageName package
              <> "/preferred"
          )

    logDebug
      $ "Hackage dependency details"
      :# [ "package" .= unPackageName package
         , "versions" .= (hvNormal <$> eVersions)
         ]

    pure $ hush eVersions

instance MonadIO m => MonadStackage (AppT app m) where
  getStackageVersions resolver package = do
    eStackageVersions <-
      httpParse parseStackageVersions
        =<< liftIO
          ( parseRequest
              $ unpack
              $ "https://www.stackage.org/"
              <> unStackageResolver resolver
              <> "/package/"
              <> unPackageName package
          )

    logDebug
      $ "Stackage dependency details"
      :# [ "resolver" .= unStackageResolver resolver
         , "package" .= unPackageName package
         , "versions" .= eStackageVersions
         ]

    pure $ hush eStackageVersions

runAppT :: (MonadUnliftIO m, HasLogger app) => AppT app m a -> app -> m a
runAppT action app =
  runLoggerLoggingT app $ runReaderT (unAppT action) app

data App = App
  { appOptions :: Options
  , appLogger :: Logger
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

httpParse
  :: MonadIO m
  => (BSL.ByteString -> Either String a)
  -> Request
  -> m (Either String a)
httpParse f req = do
  resp <- httpLBS req
  pure $ do
    note "Non-200 status" $ guard $ getResponseStatus resp == status200
    f $ getResponseBody resp
