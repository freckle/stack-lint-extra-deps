module Lsd.Checks
  ( ChecksName(..)
  , readChecksName
  , checksNameList
  , checksByName
  ) where

import RIO

import Lsd.Check
import Lsd.Checks.GitVersion
import Lsd.Checks.HackageVersion
import Lsd.Checks.RedundantGit
import Lsd.Checks.RedundantHackage

data ChecksName
    = AllChecks
    | GitChecks
    | HackageChecks

readChecksName :: String -> Either String ChecksName
readChecksName = \case
  "all" -> Right AllChecks
  "git" -> Right GitChecks
  "hackage" -> Right HackageChecks
  x -> Left $ "Invalid checks name: " <> x

checksNameList :: String
checksNameList = "all, git, hackage"

checksByName :: ChecksName -> [Check]
checksByName = \case
  AllChecks -> checksByName GitChecks <> checksByName HackageChecks
  GitChecks -> [checkRedundantGit, checkGitVersion]
  HackageChecks -> [checkRedundantHackage, checkHackageVersion]
