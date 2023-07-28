{-# LANGUAGE TupleSections #-}

module SLED.GitDetails
  ( GitDetails (..)
  , getGitDetails
  ) where

import RIO

import qualified RIO.ByteString.Lazy as BSL
import RIO.Char (isSpace)
import RIO.Directory (withCurrentDirectory)
import RIO.Process
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import SLED.GitExtraDep
import SLED.Version

data GitDetails = GitDetails
  { gdHeadCommit :: CommitSHA
  , gdCommitCountToHead :: Int
  , gdCommitCountToVersionTags :: [(Int, Version)]
  }

getGitDetails
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => GitExtraDep
  -> m (Maybe GitDetails)
getGitDetails GitExtraDep {..} = do
  logDebug $ "Cloning " <> fromString cloneUrl <> "..."

  withSystemTempDirectory "stack-lint-extra-deps" $ \path -> do
    proc "git" ["clone", "--quiet", cloneUrl, path] runProcess_

    withCurrentDirectory path $ do
      commit <- gitRevParse "HEAD"
      countToHead <- gitCountRevisionBetween gedCommit $ CommitSHA "HEAD"
      countToVersionTags <- do
        pairs <- gitTaggedVersions
        forMaybeM pairs $ \(sha, version) -> do
          mCountBehind <- gitCountRevisionBetween sha gedCommit
          mCountAhead <- gitCountRevisionBetween gedCommit sha

          let mCount = do
                behind <- mCountBehind
                ahead <- mCountAhead
                pure $ if behind > ahead then negate behind else ahead

          pure $ (,version) <$> mCount

      logDebug
        $ "Git details for "
        <> display gedRepository
        <> ":"
        <> "\n  HEAD: "
        <> display commit
        <> "\n  Commits to HEAD: "
        <> maybe "unknown" displayShow countToHead
        <> "\n  Versions by tags: "
        <> displayVersions countToVersionTags

      pure $ GitDetails commit <$> countToHead <*> pure countToVersionTags
 where
  cloneUrl = unpack $ unRepository gedRepository

displayVersions :: [(Int, Version)] -> Utf8Builder
displayVersions =
  display
    . T.intercalate ", "
    . map
      (\(n, v) -> pack $ showVersion v <> " (" <> show n <> " commits)")

gitRevParse
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => String
  -> m CommitSHA
gitRevParse ref = do
  bs <- proc "git" ["rev-parse", ref] readProcessStdout_
  pure $ CommitSHA $ T.strip $ bsToText bs

gitCountRevisionBetween
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => CommitSHA
  -> CommitSHA
  -> m (Maybe Int)
gitCountRevisionBetween a b =
  bsToInt <$> proc "git" ["rev-list", "--count", spec] readProcessStdout_
 where
  spec = unpack $ unCommitSHA a <> ".." <> unCommitSHA b

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
      version <- parseVersion $ unpack $ T.dropPrefix "v" tag
      pure (CommitSHA sha, version)
    _ -> Nothing

  -- https://stackoverflow.com/a/47447334
  refFormat :: String
  refFormat =
    "--format=%(refname) %(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end)"

bsToInt :: BSL.ByteString -> Maybe Int
bsToInt = readMaybe . unpack . T.dropWhileEnd isSpace . bsToText

bsToText :: BSL.ByteString -> Text
bsToText = decodeUtf8With lenientDecode . BSL.toStrict
