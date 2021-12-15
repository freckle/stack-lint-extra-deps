module Lsd.Cache
  ( HasCache(..)
  , defaultCacheDirectory
  , cachedFile
  , cachedDirectory
  , withinCachedDirectory
  ) where

import RIO

import qualified RIO.ByteString.Lazy as BSL
import RIO.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , withCurrentDirectory
  )
import RIO.FilePath ((</>))
import System.Environment.XDG.BaseDir

class HasCache env where
    cacheEnabledL :: Lens' env Bool
    cacheDirectoryL :: Lens' env FilePath

defaultCacheDirectory :: MonadIO m => m FilePath
defaultCacheDirectory = liftIO $ getUserCacheDir "lsd"

cachedFile
  :: (MonadUnliftIO m, MonadReader env m, HasCache env)
  => FilePath
  -> m (Maybe BSL.ByteString)
  -> m (Maybe BSL.ByteString)
cachedFile name action = do
  enabled <- view cacheEnabledL
  directory <- view cacheDirectoryL
  createDirectoryIfMissing True directory

  let path = directory </> name
  exists <- doesFileExist path

  if exists && enabled
    then Just . BSL.fromStrict <$> readFileBinary path
    else do
      mResult <- action
      mResult <$ traverse_ (writeFileBinary path . BSL.toStrict) mResult

cachedDirectory
  :: (MonadIO m, MonadReader env m, HasCache env)
  => FilePath
  -> (FilePath -> m ())
  -> m FilePath
cachedDirectory name create = do
  enabled <- view cacheEnabledL
  directory <- view cacheDirectoryL
  createDirectoryIfMissing True directory

  let path = directory </> name
  exists <- doesDirectoryExist path

  if exists
    then unless enabled $ do
      removeDirectoryRecursive path
      create path
    else create path

  pure path

withinCachedDirectory
  :: (MonadUnliftIO m, MonadReader env m, HasCache env)
  => FilePath
  -> (FilePath -> m ())
  -> m a
  -> m a
withinCachedDirectory name create act = do
  path <- cachedDirectory name create
  withCurrentDirectory path act
