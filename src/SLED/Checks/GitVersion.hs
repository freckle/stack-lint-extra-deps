module SLED.Checks.GitVersion
  ( checkGitVersion
  ) where

import SLED.Prelude

import SLED.Check

checkGitVersion :: Check
checkGitVersion = Check $ \ed _ -> do
  gd <- ed.gitDetails
  UpdateGitCommit gd.headCommit <$ guard (gd.commitCountToHead > 0)
