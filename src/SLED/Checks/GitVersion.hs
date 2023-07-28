module SLED.Checks.GitVersion
  ( checkGitVersion
  ) where

import RIO

import SLED.Check

checkGitVersion :: Check
checkGitVersion = Check $ \ExternalDetails {..} extraDep -> do
  Git ged <- pure extraDep
  GitDetails {..} <- edGitDetails

  guard $ gdCommitCountToHead >= 1

  pure
    $ Suggestion
      { sTarget = extraDep
      , sAction = ReplaceWith $ Git $ ged {gedCommit = gdHeadCommit}
      , sDescription =
          "There are newer commits ("
            <> displayShow gdCommitCountToHead
            <> ") on the default branch"
      }
