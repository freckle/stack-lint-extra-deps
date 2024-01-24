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

  let
    areNewerCommits :: Int -> Text
    areNewerCommits = \case
      1 -> "is one newer commit"
      n -> "are " <> pack (show n) <> " newer commits"

  pure
    $ Suggestion
      { action = ReplaceCommit ged.commit gd.headCommit
      , reason =
          "There " <> areNewerCommits gd.commitCountToHead <> " on the default branch"
      }
