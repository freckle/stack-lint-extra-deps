module SLED.App
  ( AppT (..)
  , runAppT
  ) where

import SLED.Prelude

import Control.Error.Util (hush, note)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (status200)
import SLED.GitDetails
import SLED.Hackage
import SLED.PackageName
import SLED.Stackage
import SLED.StackageResolver
import System.Process.Typed
import UnliftIO.Exception (throwIO)

newtype AppT app m a = AppT
  { unwrap :: ReaderT app (LoggingT m) a
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

instance MonadIO m => MonadThrow (AppT app m) where
  throwM = throwIO

instance MonadIO m => MonadHackage (AppT app m) where
  getHackageVersions package = do
    eVersions <-
      httpParse eitherDecode
        . addRequestHeader hAccept "application/json"
        =<< liftIO
          ( parseRequest
              $ unpack
              $ "https://hackage.haskell.org/package/"
              <> package.unwrap
              <> "/preferred"
          )

    logDebug
      $ "Hackage dependency details"
      :# [ "package" .= package
         , "versions" .= ((.normal) <$> eVersions)
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
              <> resolver.unwrap
              <> "/package/"
              <> package.unwrap
          )

    logDebug
      $ "Stackage dependency details"
      :# [ "resolver" .= resolver
         , "package" .= package
         , "versions" .= eStackageVersions
         ]

    pure $ hush eStackageVersions

instance MonadIO m => MonadGit (AppT app m) where
  gitClone url path = runGit ["clone", "--quiet", url, path]
  gitRevParse ref = readGit ["rev-parse", ref]
  gitRevListCount spec = readGit ["rev-list", "--count", spec]
  gitForEachRef refFormat = readGit ["for-each-ref", refFormat, "refs/tags"]

runAppT :: (MonadUnliftIO m, HasLogger app) => AppT app m a -> app -> m a
runAppT action app =
  runLoggerLoggingT app $ runReaderT action.unwrap app

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

runGit :: (MonadIO m, MonadLogger m) => [String] -> m ()
runGit args = do
  logDebug $ "git" :# ["args" .= args]
  runProcess_ $ proc "git" args

readGit :: (MonadIO m, MonadLogger m) => [String] -> m BSL.ByteString
readGit args = do
  logDebug $ "git" :# ["args" .= args]
  readProcessStdout_ $ proc "git" args
