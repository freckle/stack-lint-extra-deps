{-# LANGUAGE TupleSections #-}

module Lsd.GitDetails
  ( GitDetails(..)
  , getGitDetails
  ) where

import RIO

import Lsd.GitExtraDep
import Lsd.Version
import qualified RIO.ByteString.Lazy as BSL
import RIO.Char (isSpace)
import RIO.Directory (withCurrentDirectory)
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

data GitDetails = GitDetails
  { gdCommitCountToHead :: Int
  , gdCommitCountToVersionTags :: [(Int, Version)]
  }

getGitDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => GitExtraDep
  -> m (Maybe GitDetails)
getGitDetails GitExtraDep {..} = do
  logDebug $ "Cloning " <> fromString cloneUrl <> "..."

  withSystemTempDirectory "lsd" $ \path -> do
    proc "git" ["clone", "--quiet", cloneUrl, path] runProcess_

    withCurrentDirectory path $ do
      countToHead <- gitCountRevisionBetween gedCommit $ CommitSHA "HEAD"
      countToVersionTags <- do
        pairs <- gitTaggedVersions
        forMaybeM pairs $ \(sha, version) -> do
          mCount <- gitCountRevisionBetween gedCommit sha
          pure $ (, version) <$> mCount

      logDebug
        $ "Git details for "
        <> display gedRepository
        <> ":"
        <> "\n  Count to HEAD: "
        <> displayShow countToHead
        <> "\n  Count to version tags: "
        <> displayShow countToVersionTags

      pure $ GitDetails <$> countToHead <*> pure countToVersionTags
  where cloneUrl = unpack $ unRepository gedRepository

gitCountRevisionBetween
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => CommitSHA
  -> CommitSHA
  -> m (Maybe Int)
gitCountRevisionBetween a b =
  bsToInt <$> proc "git" ["rev-list", "--count", spec] readProcessStdout_
  where spec = unpack $ unCommitSHA a <> ".." <> unCommitSHA b

gitTaggedVersions
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => m [(CommitSHA, Version)]
gitTaggedVersions = do
  bs <- proc "git" ["for-each-ref", refFormat, "refs/tags"] readProcessStdout_
  pure $ mapMaybe toPair $ T.lines $ bsToText bs
 where
  -- naively parse "refs/tags/{tag} {sha}"
  toPair :: Text -> Maybe (CommitSHA, Version)
  toPair x = case T.words x of
    [ref, sha] -> do
      tag <- T.stripPrefix "refs/tags/" ref
      version <- parseVersion $ unpack tag
      pure (CommitSHA sha, version)

    _ -> Nothing

  -- https://stackoverflow.com/a/47447334
  refFormat :: String
  refFormat
    = "--format=%(refname) %(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end)"

bsToInt :: BSL.ByteString -> Maybe Int
bsToInt = readMaybe . unpack . T.dropWhileEnd isSpace . bsToText

bsToText :: BSL.ByteString -> Text
bsToText = decodeUtf8With lenientDecode . BSL.toStrict
