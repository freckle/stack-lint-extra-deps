module SLED.Checks
  ( ChecksName (..)
  , checksNameOption
  , checksByName
  ) where

import RIO

import Options.Applicative
import SLED.Check
import SLED.Checks.GitVersion
import SLED.Checks.HackageVersion
import SLED.Checks.RedundantGit
import SLED.Checks.RedundantHackage
import SLED.Options.BoundedEnum

data ChecksName
  = AllChecks
  | GitChecks
  | HackageChecks
  deriving stock (Bounded, Enum)

checksNameOption :: Parser ChecksName
checksNameOption =
  boundedEnumOptionWith
    showChecksName
    ( \list ->
        long "checks"
          <> help ("Checks to run, one of: " <> list)
          <> metavar "CHECKS"
          <> value AllChecks
    )

showChecksName :: ChecksName -> String
showChecksName = \case
  AllChecks -> "all"
  GitChecks -> "git"
  HackageChecks -> "hackage"

checksByName :: ChecksName -> [Check]
checksByName = \case
  AllChecks -> checksByName GitChecks <> checksByName HackageChecks
  GitChecks -> [checkRedundantGit, checkGitVersion]
  HackageChecks -> [checkRedundantHackage, checkHackageVersion]
