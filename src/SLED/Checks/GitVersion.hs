module SLED.Checks.GitVersion
  ( checkGitVersion
  ) where

import SLED.Prelude

import SLED.Check

checkGitVersion :: Check
checkGitVersion = Check $ \ExternalDetails {..} extraDep -> do
  Git ged <- pure $ markedItem extraDep
  GitDetails {..} <- edGitDetails

  guard $ gdCommitCountToHead >= 1

  pure
    $ Suggestion
      { sTarget = extraDep
      , sAction = replaceGitExtraDepCommit ged gdHeadCommit
      , sDescription =
          "There are newer commits ("
            <> pack (show gdCommitCountToHead)
            <> ") on the default branch"
      }
