{-# LANGUAGE TupleSections #-}

module SLED.GitDetails
  ( GitDetails (..)
  , getGitDetails
  ) where

import SLED.Prelude

import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.List.Extra (dropPrefix)
import qualified Data.Text as T
import SLED.GitExtraDep
import SLED.Version
import System.Process.Typed
import UnliftIO.Directory (withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

data GitDetails = GitDetails
  { gdHeadCommit :: CommitSHA
  , gdCommitCountToHead :: Int
  , gdCommitCountToVersionTags :: [(Int, Version)]
  }

getGitDetails
  :: (MonadUnliftIO m, MonadLogger m)
  => GitExtraDep
  -> m (Maybe GitDetails)
getGitDetails GitExtraDep {..} = do
  logDebug $ "Cloning git dependency" :# ["cloneUrl" .= cloneUrl]

  withSystemTempDirectory "stack-lint-extra-deps" $ \path -> do
    runProcess_ $ proc "git" ["clone", "--quiet", cloneUrl, path]

    withCurrentDirectory path $ do
      commit <- gitRevParse "HEAD"
      countToHead <- gitCountRevisionBetween gedCommit $ CommitSHA "HEAD"
      countToVersionTags <- do
        pairs <- gitTaggedVersions
        fmap catMaybes $ for pairs $ \(sha, version) -> do
          mCountBehind <- gitCountRevisionBetween sha gedCommit
          mCountAhead <- gitCountRevisionBetween gedCommit sha

          let mCount = do
                behind <- mCountBehind
                ahead <- mCountAhead
                pure $ if behind > ahead then negate behind else ahead

          pure $ (,version) <$> mCount

      logDebug
        $ "Git dependency details"
        :# [ "repository" .= gedRepository
           , "headCommit" .= commit
           , "commitsToHead" .= (show @Text <$> countToHead)
           , "versionsByTags" .= countToVersionTags
           ]

      pure $ GitDetails commit <$> countToHead <*> pure countToVersionTags
 where
  cloneUrl = unpack $ unRepository gedRepository

gitRevParse
  :: (MonadIO m, MonadLogger m)
  => String
  -> m CommitSHA
gitRevParse ref = do
  bs <- readProcessStdout_ $ proc "git" ["rev-parse", ref]
  pure $ CommitSHA $ T.strip $ bsToText bs

gitCountRevisionBetween
  :: (MonadIO m, MonadLogger m)
  => CommitSHA
  -> CommitSHA
  -> m (Maybe Int)
gitCountRevisionBetween a b =
  bsToInt <$> readProcessStdout_ (proc "git" ["rev-list", "--count", spec])
 where
  spec = unpack $ unCommitSHA a <> ".." <> unCommitSHA b

gitTaggedVersions
  :: (MonadIO m, MonadLogger m)
  => m [(CommitSHA, Version)]
gitTaggedVersions = do
  bs <- readProcessStdout_ $ proc "git" ["for-each-ref", refFormat, "refs/tags"]
  pure $ mapMaybe toPair $ T.lines $ bsToText bs
 where
  -- naively parse "refs/tags/{tag} {sha}"
  toPair :: Text -> Maybe (CommitSHA, Version)
  toPair x = case T.words x of
    [ref, sha] -> do
      tag <- T.stripPrefix "refs/tags/" ref
      version <- parseVersion $ dropPrefix "v" $ unpack tag
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
