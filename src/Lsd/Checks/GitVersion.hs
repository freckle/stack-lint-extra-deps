module Lsd.Checks.GitVersion
    ( checkGitVersion
    ) where

import RIO

import Lsd.Cache
import Lsd.Check
import Lsd.Checks.Git
import Lsd.GitExtraDep
import Lsd.Suggestion
import qualified RIO.ByteString.Lazy as BSL
import RIO.Char (isSpace)
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

checkGitVersion
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasCache env
       )
    => Check m
checkGitVersion = checkInGitClone gitRevListToHead $ \n -> do
    guard $ n >= 1
    pure $ Suggestion
        { sAction = Replace
        , sDetails =
            "There are newer commits ("
            <> displayShow n
            <> ") on the default branch"
        }

gitRevListToHead
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => GitExtraDep
    -> m (Maybe Int)
gitRevListToHead GitExtraDep {..} =
    bsToInt
        <$> proc
                "git"
                ["rev-list", "--count", commit <> "..HEAD"]
                readProcessStdout_
    where commit = unpack $ unCommitSHA gedCommit

bsToInt :: BSL.ByteString -> Maybe Int
bsToInt =
    readMaybe
        . unpack
        . T.dropWhileEnd isSpace
        . decodeUtf8With lenientDecode
        . BSL.toStrict
