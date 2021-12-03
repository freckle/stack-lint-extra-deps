module Lsd.Checks.RedundantGit
  ( checkRedundantGit
  ) where

import RIO

import Control.Monad.Extra (firstJustM)
import Lsd.Cache
import Lsd.Check
import Lsd.Checks.Git
import Lsd.GitExtraDep
import Lsd.Suggestion
import Lsd.Version
import qualified RIO.ByteString.Lazy as BSL
import RIO.Char (isSpace)
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

checkRedundantGit
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasCache env
     )
  => Check m
checkRedundantGit = checkInGitClone gitTagInHackage $ \version -> do
  pure $ Suggestion
    { sAction = Replace
    , sDetails = "Newer, version-like tag (" <> display version <> ") exists"
    }

gitTagInHackage
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => GitExtraDep
  -> m (Maybe Version)
gitTagInHackage GitExtraDep {..} = do
  pairs <- gitTaggedVersions
  tags <- map fst <$> filterM ((`shaIsSameOrNewer` gedCommit) . snd) pairs
  firstJustM guardExistsInHackage $ reverse tags

gitTaggedVersions
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => m [(Version, CommitSHA)]
gitTaggedVersions = do
  bs <- proc "git" ["for-each-ref", refFormat, "refs/tags"] readProcessStdout_
  pure $ mapMaybe toPair $ T.lines $ decodeUtf8With lenientDecode $ BSL.toStrict
    bs
 where
    -- naively parse "refs/tags/{tag} {sha}"
  toPair :: Text -> Maybe (Version, CommitSHA)
  toPair x = case T.words x of
    [ref, sha] -> do
      tag <- T.stripPrefix "refs/tags/" ref
      version <- parseVersion $ unpack tag
      pure (version, CommitSHA sha)

    _ -> Nothing

  -- https://stackoverflow.com/a/47447334
  refFormat :: String
  refFormat
    = "--format=%(refname) %(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end)"

guardExistsInHackage :: MonadIO m => Version -> m (Maybe Version)
guardExistsInHackage = pure . Just -- TODO

shaIsSameOrNewer
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => CommitSHA
  -> CommitSHA
  -> m Bool
a `shaIsSameOrNewer` b = do
  n <-
    bsToInt
      <$> proc
            "git"
            ["rev-list", "--count", un a <> ".." <> un b]
            readProcessStdout_
  pure $ maybe False (>= 0) n
  where un = unpack . unCommitSHA

bsToInt :: BSL.ByteString -> Maybe Int
bsToInt =
  readMaybe
    . unpack
    . T.dropWhileEnd isSpace
    . decodeUtf8With lenientDecode
    . BSL.toStrict
