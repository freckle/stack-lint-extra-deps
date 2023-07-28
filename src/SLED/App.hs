{-# LANGUAGE TupleSections #-}

module SLED.App
  ( AppT (..)
  , runAppT

    -- * Our concrete App
  , App (..)
  ) where

import SLED.Prelude

import Control.Error.Util (hush)
import Data.Aeson (eitherDecode)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (Status (..), status200)
import SLED.Hackage
import SLED.Options
import SLED.PackageName
import SLED.StackYaml
import SLED.Stackage
import SLED.StackageResolver
import SLED.Version
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

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
    req <-
      liftIO
        $ parseRequest
        $ unpack
        $ "https://hackage.haskell.org/package/"
        <> unPackageName package
        <> "/preferred"

    -- We'll parse ourselves because it makes logging more convenient
    resp <- httpLBS $ addRequestHeader hAccept "application/json" req

    let
      mBody = do
        guard $ getResponseStatus resp == status200
        pure $ getResponseBody resp
      eVersions = do
        body <- mBody
        eitherDecode body

    logDebug
      $ "Hackage dependency details"
      :# [ "package" .= unPackageName package
         , "statusCode" .= statusCode (getResponseStatus resp)
         , "versions" .= (hvNormal <$> eVersions)
         ]

    pure $ hush eVersions

instance MonadIO m => MonadStackage (AppT app m) where
  getStackageVersions resolver package = do
    req <-
      liftIO
        $ parseRequest
        $ unpack
        $ "https://www.stackage.org/"
        <> unStackageResolver resolver
        <> "/package/"
        <> unPackageName package

    resp <- httpLBS req

    let
      mBody = do
        guard $ getResponseStatus resp == status200
        pure $ getResponseBody resp
      mVersions = parseVersionsTable . fromDocument . parseLBS <$> mBody

    logDebug
      $ "Stackage dependency details"
      :# [ "package" .= unPackageName package
         , "statusCode" .= statusCode (getResponseStatus resp)
         , "versions" .= mVersions
         ]

    pure $ do
      versions <- mVersions
      StackageVersions
        <$> Map.lookup currentKey versions
        <*> Map.lookup latestKey versions
   where
    parseVersionsTable :: Cursor -> Map Text Version
    parseVersionsTable cursor = do
      fixNightly
        $ Map.fromList
        $ mapMaybe (toPair . ($// content))
        $ cursor
        $// element "tr"
     where
      toPair = \case
        [] -> Nothing
        [_] -> Nothing
        [k, v] -> (k,) <$> parseVersion (unpack v)
        [k, _, v] -> (k,) <$> parseVersion (unpack v)
        (k : v : _) -> (k,) <$> parseVersion (unpack v)

      fixNightly m =
        maybe m (\(_, v) -> Map.insertWith (\_new old -> old) currentKey v m)
          $ find ((nightlyPrefix `T.isPrefixOf`) . fst)
          $ Map.toList m

    currentKey :: Text
    currentKey = "Version on this page:"

    latestKey :: Text
    latestKey = "Latest on Hackage:"

    nightlyPrefix :: Text
    nightlyPrefix = "Stackage Nightly "

runAppT :: (MonadUnliftIO m, HasLogger app) => AppT app m a -> app -> m a
runAppT action app =
  runLoggerLoggingT app $ runReaderT (unAppT action) app

data App = App
  { appOptions :: Options
  , appLogger :: Logger
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}
