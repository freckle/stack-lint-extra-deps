module Checks.GitVersion
  ( checkGitVersion
  ) where

import RIO

import Check

checkGitVersion :: Check
checkGitVersion = Check $ \ExternalDetails {..} extraDep -> do
  Git _ <- pure extraDep
  GitDetails {..} <- edGitDetails

  guard $ gdCommitCountToHead >= 1

  pure $ Suggestion
    { sAction = Replace
    , sDetails =
      "There are newer commits ("
      <> displayShow gdCommitCountToHead
      <> ") on the default branch"
    }
