module SLED.Checks.GitVersion
  ( checkGitVersion
  ) where

import SLED.Prelude

import SLED.Check

checkGitVersion :: Check
checkGitVersion = Check $ \ed extraDep -> do
  Git ged <- pure $ markedItem extraDep
  gd <- ed.gitDetails

  guard $ gd.commitCountToHead >= 1

  pure
    $ Suggestion
      { target = extraDep
      , action = replaceGitExtraDepCommit ged gd.headCommit
      , description =
          "There are newer commits ("
            <> pack (show gd.commitCountToHead)
            <> ") on the default branch"
      }
