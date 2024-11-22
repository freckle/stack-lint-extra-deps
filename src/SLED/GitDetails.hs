{-# LANGUAGE TupleSections #-}

module SLED.GitDetails
  ( MonadGit (..)
  , GitDetails (..)
  , getGitDetails
  ) where

import SLED.Prelude

import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import qualified Data.Text as T
import SLED.GitExtraDep
import SLED.Version
import UnliftIO.Directory (withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

class Monad m => MonadGit m where
  gitClone :: String -> FilePath -> m ()
  gitRevParse :: String -> m BSL.ByteString
  gitRevListCount :: String -> m BSL.ByteString
  gitForEachRef :: String -> m BSL.ByteString

data GitDetails = GitDetails
  { headCommit :: CommitSHA
  , commitCountToHead :: Int
  , commitCountToVersionTags :: [(Int, Version)]
  }

getGitDetails
  :: (MonadUnliftIO m, MonadLogger m, MonadGit m)
  => GitExtraDep
  -> m (Maybe GitDetails)
getGitDetails ged = do
  logDebug $ "Cloning git dependency" :# ["cloneUrl" .= cloneUrl]

  withSystemTempDirectory "stack-lint-extra-deps" $ \path -> do
    gitClone cloneUrl path

    withCurrentDirectory path $ do
      commit <- gitCurrentSHA "HEAD"
      countToHead <-
        gitCountRevisionBetween (markedItem ged.commit) $ CommitSHA "HEAD"
      countToVersionTags <- do
        pairs <- gitTaggedVersions
        fmap catMaybes $ for pairs $ \(sha, version) -> do
          mCountBehind <- gitCountRevisionBetween sha (markedItem ged.commit)
          mCountAhead <- gitCountRevisionBetween (markedItem ged.commit) sha

          let mCount = do
                behind <- mCountBehind
                ahead <- mCountAhead
                pure $ if behind > ahead then negate behind else ahead

          pure $ (,version) <$> mCount

      logDebug
        $ "Git dependency details"
        :# [ "repository" .= ged.repository
           , "headCommit" .= commit
           , "commitsToHead" .= (show @Text <$> countToHead)
           , "versionsByTags" .= countToVersionTags
           ]

      pure $ GitDetails commit <$> countToHead <*> pure countToVersionTags
 where
  cloneUrl = unpack ged.repository.unwrap

gitCurrentSHA
  :: MonadGit m
  => String
  -> m CommitSHA
gitCurrentSHA ref = do
  bs <- gitRevParse ref
  pure $ CommitSHA $ T.strip $ bsToText bs

gitCountRevisionBetween
  :: MonadGit m
  => CommitSHA
  -> CommitSHA
  -> m (Maybe Int)
gitCountRevisionBetween a b =
  bsToInt <$> gitRevListCount spec
 where
  spec = unpack $ a.unwrap <> ".." <> b.unwrap

gitTaggedVersions
  :: MonadGit m
  => m [(CommitSHA, Version)]
gitTaggedVersions = do
  bs <- gitForEachRef refFormat
  pure $ mapMaybe toPair $ T.lines $ bsToText bs
 where
  -- naively parse "refs/tags/{tag} {sha}"
  toPair :: Text -> Maybe (CommitSHA, Version)
  toPair x = case T.words x of
    [ref, sha] -> do
      tag <- T.stripPrefix "refs/tags/" ref
      version <- parseVersion $ dropPrefix "v" tag
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

dropPrefix :: Text -> Text -> Text
dropPrefix p t = fromMaybe t $ T.stripPrefix p t
