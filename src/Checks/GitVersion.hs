module Checks.GitVersion
  ( checkGitVersion
  ) where

import RIO

import Check

checkGitVersion :: Check
checkGitVersion = Check $ \ExternalDetails {..} extraDep -> do
  Git ged <- pure extraDep
  GitDetails {..} <- edGitDetails

  guard $ gdCommitCountToHead >= 1

  pure $ Suggestion
    { sTarget = extraDep
    , sAction = ReplaceWith $ Git $ ged { gedCommit = gdHeadCommit }
    , sDetails =
      "There are newer commits ("
      <> displayShow gdCommitCountToHead
      <> ") on the default branch"
    }
